module Network.Socket(
         Socket
       , Family(..)
       , isSupportedFamily
       , SocketType(..)
       , isSupportedSocketType
       , SockAddr(..)
       , isSupportedSockAddr
       , SocketStatus(..)
       , HostAddress
       , hostAddressToTuple
       , tupleToHostAddress
       , FlowInfo
       , ScopeID
       , htonl
       , ntohl
       , ShutdownCmd(..)
       , ProtocolNumber
       , defaultProtocol
       , PortNumber(..)
       , HostName
       , ServiceName
       , AddrInfo(..)
       , AddrInfoFlag(..)
       , addrInfoFlagImplemented
       , defaultHints
       , getAddrInfo
       , NameInfoFlag(..)
       , getNameInfo
       , socket
       , socketPair
       , connect
       , bind
       , listen
       , accept
       , getPeerName
       , getSocketName
       , getPeerCred
       , socketPort
       , socketToHandle
       , send
       , sendTo
       , recv
       , recvFrom
       , recvLen
       , sendBuf
       , recvBuf
       , sendBufTo
       , recvBufFrom
       , inet_addr
       , inet_ntoa
       , shutdown
       , close
       , isConnected
       , isBound
       , isListening
       , isReadable
       , isWritable
       , SocketOption(..)
       , isSupportedSocketOption
       , getSocketOption
       , setSocketOption
       , sendFd
       , recvFd
       , aNY_PORT
       , iNADDR_ANY
       , iN6ADDR_ANY
       , sOMAXCONN
       , sOL_SOCKET
       , sCM_RIGHTS
       , maxListenQueue
       , withSocketsDo
       , sClose
       )
 where

import           Control.Concurrent.MVar()
import           Control.Exception(SomeException, throwIO, catch)
import           Data.Bits(shiftR, shiftL, (.|.), (.&.))
import           Data.ByteString(useAsCStringLen, packCStringLen)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString.Lazy.Char8(pack, unpack)
import           Data.Typeable(Typeable)
import           Data.Word(Word8, Word32)
import           Foreign.C.Types(CUInt, CInt)
import           Foreign.Ptr(Ptr, castPtr)
import           Foreign.Storable()
import           Hans.IP4(packIP4, unpackIP4)
import           Hans.Lens(view)
import           Hans.Socket(tcpRemoteAddr, tcpRemotePort,
                             tcpLocalAddr, tcpLocalPort)
import           Hans.Socket.Handle()
import           Network.BSD(ProtocolNumber, ServiceEntry(..), ServiceName,
                             HostName, hostAddress, getHostByName,HostEntry(..),
                             getHostByAddr, ProtocolEntry(..),
                             getProtocolByNumber, getProtocolByName,
                             getServiceByName)
import qualified Network.Socket.ByteString as NBS
import           Network.Socket.Internal(HostAddress, HostAddress6, Family(..),
                                         PortNumber(..), SockAddr(..))
import qualified Network.Socket.ByteString.Lazy as SL
import           Network.Socket.Types(Socket, SocketStatus(..), Direction(..),
                                      withTcpSocket, close, fromIP4, toIP4,
                                      getSocketStatus, directionOpen,
                                      newSocket, connectIP4, bindIP4,
                                      listen, acceptIP4,
                                      socketToHandle, ShutdownCmd(..), shutdown)

isSupportedFamily :: Family -> Bool
isSupportedFamily AF_INET = True
isSupportedFamily _       = False

data SocketType = NoSocketType
                | Stream
                | Datagram
                | Raw
                | RDM
                | SeqPacket
 deriving (Eq, Ord, Read, Show)

isSupportedSocketType :: SocketType -> Bool
isSupportedSocketType Stream   = True
isSupportedSocketType Datagram = True
isSupportedSocketType _        = False

isSupportedSockAddr :: SockAddr -> Bool
isSupportedSockAddr _ = True

hostAddressToTuple :: HostAddress -> (Word8, Word8, Word8, Word8)
hostAddressToTuple = unpackIP4 . toIP4

tupleToHostAddress :: (Word8, Word8, Word8, Word8) -> HostAddress
tupleToHostAddress (a, b, c, d) = fromIP4 (packIP4 a b c d)

type FlowInfo = Word32

type ScopeID = Word32

htonl :: Word32 -> Word32
htonl = bswap32

ntohl :: Word32 -> Word32
ntohl = bswap32

bswap32 :: Word32 -> Word32
bswap32 x = (((x) .&. 0xFF000000) `shiftR` 24) .|.
            (((x) .&. 0x00FF0000) `shiftR`  8) .|.
            (((x) .&. 0x0000FF00) `shiftL`  8) .|.
            (((x) .&. 0x000000FF) `shiftL` 24)

defaultProtocol :: ProtocolNumber
defaultProtocol  = 6

data AddrInfo = AddrInfo {
        addrFlags      :: [AddrInfoFlag]
      , addrFamily     :: Family
      , addrSocketType :: SocketType
      , addrProtocol   :: ProtocolNumber
      , addrAddress    :: SockAddr
      , addrCanonName  :: Maybe String
      }
    deriving (Eq, Show, Typeable)

data AddrInfoFlag = AI_ADDRCONFIG | AI_ALL | AI_CANONNAME | AI_NUMERICHOST
                  | AI_NUMERICSERV | AI_PASSIVE | AI_V4MAPPED
  deriving (Eq, Read, Show, Typeable)

addrInfoFlagImplemented :: AddrInfoFlag -> Bool
addrInfoFlagImplemented _ = False

defaultHints :: AddrInfo
defaultHints  = AddrInfo [] AF_UNSPEC NoSocketType defaultProtocol undefined undefined

getAddrInfo :: Maybe AddrInfo -> Maybe HostName -> Maybe ServiceName ->
               IO [AddrInfo]
getAddrInfo _ Nothing Nothing =
  throwIO (userError "Cannot pass two Nothings to getAddrInfo")
getAddrInfo Nothing mhn msn =
  getAddrInfo (Just defaultHints) mhn msn
getAddrInfo mai Nothing msn =
  getAddrInfo mai (Just "127.0.0.1") msn
getAddrInfo (Just hints) (Just hostname) msn =
  do addr  <- case reads hostname of
                [(addr, "")] -> return (fromIP4 addr)
                _            -> hostAddress `fmap` (getHostByName hostname)
     (port, proto) <- case msn of
                        Nothing -> return (0, defaultProtocol)
                        Just _ ->
                          do pent <- getProtocolByNumber (addrProtocol hints)
                             entry <- getServiceByName "sn" (protoName pent)
                             let port = servicePort entry
                             prot <- getProtocolByName (serviceProtocol entry)
                             return (port, protoNumber prot)
     let stype = if proto == 6 then Stream else Datagram
     -- Now figure out the socket type
     let sockaddr = SockAddrInet port addr
     return [hints{ addrFamily    = AF_INET, addrSocketType = stype,
                    addrProtocol  = proto, addrAddress = sockaddr,
                    addrCanonName = Just hostname }]

data NameInfoFlag = NI_DGRAM | NI_NAMEREQD | NI_NOFQDN
                  | NI_NUMERICHOST | NI_NUMERICSERV
  deriving (Eq, Read, Show, Typeable)

getNameInfo :: [NameInfoFlag] -> Bool -> Bool -> SockAddr ->
               IO (Maybe HostName, Maybe ServiceName)
getNameInfo _ _ _ _ =
  throwIO (userError "FIXME: ERROR: network-hans does not support getNameInfo")

socketPair :: Family -> SocketType -> ProtocolNumber -> IO (Socket, Socket)
socketPair _ _ _  =
  throwIO (userError "FIXME: ERROR: network-hans does not support socketPair")

connect :: Socket -> SockAddr -> IO ()
connect sock (SockAddrInet (PortNum port) addr) =
  connectIP4 sock (toIP4 addr) (fromIntegral port)

bind :: Socket -> SockAddr -> IO ()
bind sock (SockAddrInet (PortNum port) addr) =
  bindIP4 sock addr' port'
 where
  port' = if port == 0          then Nothing else Just (fromIntegral port)
  addr' = if addr == iNADDR_ANY then Nothing else Just (toIP4 addr)

accept :: Socket -> IO (Socket, SockAddr)
accept sock =
  do (sock', addr, port) <- acceptIP4 sock
     return (sock', SockAddrInet (PortNum (fromIntegral port)) (fromIP4 addr))

getPeerName :: Socket -> IO SockAddr
getPeerName s =
  withTcpSocket s ForNeither $ \ tcps ->
    do let addr = view tcpRemoteAddr tcps
           port = view tcpRemotePort tcps
       return (SockAddrInet (PortNum port) (fromIP4 addr))

getSocketName :: Socket -> IO SockAddr
getSocketName s = 
  withTcpSocket s ForNeither $ \ tcps ->
    do let addr = view tcpLocalAddr tcps
           port = view tcpLocalPort tcps
       return (SockAddrInet (PortNum port) (fromIP4 addr))

getPeerCred :: Socket -> IO (CUInt, CUInt, CUInt)
getPeerCred _ =
  throwIO (userError "getPeerCred not supported on HaNS")

socketPort :: Socket -> IO PortNumber
socketPort sock =
  do SockAddrInet port _ <- getSocketName sock
     return port

sendTo :: Socket -> String -> SockAddr -> IO Int
sendTo s str addr = NBS.sendTo s (BSC.pack str) addr

sendBufTo :: Socket -> Ptr a -> Int -> SockAddr -> IO Int
sendBufTo sock ptr sz addr =
  do bstr <- packCStringLen (castPtr ptr, fromIntegral sz)
     NBS.sendTo sock bstr addr

recvFrom :: Socket -> Int -> IO (String, Int, SockAddr)
recvFrom sock size =
  do (bstr, addr) <- NBS.recvFrom sock size
     return (BSC.unpack bstr, BS.length bstr, addr)

recvBufFrom :: Socket -> Ptr a -> Int -> IO (Int, SockAddr)
recvBufFrom sock dptr size =
  do (bstr, addr) <- NBS.recvFrom sock size
     sz <- useAsCStringLen bstr $ \ (sptr, amt) ->
                                   do memcpy dptr sptr amt
                                      return amt
     return (sz, addr)

send :: Socket -> String -> IO Int
send so st = fromIntegral `fmap` SL.send so (pack st)

recv :: Socket -> Int -> IO String
recv so sz = unpack `fmap` SL.recv so (fromIntegral sz)

recvLen :: Socket -> Int -> IO (String, Int)
recvLen so sz =
  do bstr <- SL.recv so (fromIntegral sz)
     return (unpack bstr, fromIntegral (BSL.length bstr))

sendBuf :: Socket -> Ptr Word8 -> Int -> IO Int
sendBuf so ptr sz =
  do bstr <- packCStringLen (castPtr ptr, fromIntegral sz)
     NBS.send so bstr

recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf so dptr sz =
  do bstr <- NBS.recv so sz
     sz' <- useAsCStringLen bstr $ \ (sptr, amt) ->
                                    do memcpy dptr sptr amt
                                       return amt
     return sz'

inet_addr :: String -> IO HostAddress
inet_addr str = hostAddress `fmap` getHostByName str

inet_ntoa :: HostAddress -> IO String
inet_ntoa addr =
  catch (hostName `fmap` getHostByAddr AF_INET addr)
        (\ (_ :: SomeException) -> return (show (toIP4 addr)))

isConnected :: Socket -> IO Bool
isConnected s = (== Connected) `fmap` getSocketStatus s

isBound :: Socket -> IO Bool
isBound s = (== Bound) `fmap` getSocketStatus s

isListening :: Socket -> IO Bool
isListening s = (== Listening) `fmap` getSocketStatus s

isReadable :: Socket -> IO Bool
isReadable s = directionOpen s ForRead

isWritable :: Socket -> IO Bool
isWritable s = directionOpen s ForWrite

socket :: Family -> SocketType -> ProtocolNumber -> IO Socket
socket AF_INET Stream   6  = newSocket True
socket AF_INET Datagram 17 = newSocket False
socket _       _        _  =
  throwIO (userError "Unsupported socket options.")

data SocketOption = Debug | ReuseAddr | Type | SoError | DontRoute | Broadcast
                  | SendBuffer | RecvBuffer | KeepAlive | OOBInline | TimeToLive
                  | MaxSegment | NoDelay | Cork | Linger | ReusePort
                  | RecvLowWater | SendLowWater | RecvTimeOut | SendTimeOut
                  | UseLoopBack | IPv6Only | CustomSockOpt (CInt, CInt)

  deriving (Show, Typeable)

isSupportedSocketOption :: SocketOption -> Bool
isSupportedSocketOption _ = False

getSocketOption :: Socket -> SocketOption -> IO Int
getSocketOption _ _ = return 0

setSocketOption :: Socket -> SocketOption -> Int -> IO ()
setSocketOption _ _ _ = return ()

sendFd :: Socket -> CInt -> IO ()
sendFd _ _ = fail "Cannot get file descriptor from Hans."

recvFd :: Socket -> IO CInt
recvFd _ = fail "Cannot get file descriptor from Hans."

aNY_PORT :: PortNumber
aNY_PORT  = 0

iNADDR_ANY :: HostAddress
iNADDR_ANY  = 0

iN6ADDR_ANY :: HostAddress6
iN6ADDR_ANY  = (0, 0, 0, 0)

sOMAXCONN :: Int
sOMAXCONN  = 128

sOL_SOCKET :: Int
sOL_SOCKET  = 1

sCM_RIGHTS :: Int
sCM_RIGHTS  = 1

maxListenQueue :: Int
maxListenQueue  = 128

withSocketsDo :: IO a -> IO a
withSocketsDo action = action

sClose :: Socket -> IO ()
sClose = close

foreign import ccall unsafe "string.h memcpy"
  memcpy :: Ptr a -> Ptr b -> Int -> IO ()

