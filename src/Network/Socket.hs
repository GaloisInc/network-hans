module Network.Socket(
         Socket
       , Family(..)
       , isSupportedFamily
       , SockAddr(..)
       , SocketStatus(..)
       , ShutdownCmd(..)
       , PortNumber(..)
       , HostName
       , ServiceName
       , AddrInfo
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
       , sendTo
       , sendBufTo
       , recvFrom
       , recvBufFrom
       , send
       , recv
       , recvLen
       , sendBuf
       , recvBuf
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

import Control.Concurrent.MVar
import Control.Exception
import Data.Typeable
import Data.Word
import Data.ByteString(useAsCStringLen, packCStringLen)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8(pack, unpack)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.IO.Handle(mkFileHandle)
import Hans.Address.IP4
import Hans.Message.Tcp(getPort)
import qualified Hans.NetworkStack as NS
import Hans.Socket.Handle
import Network.BSD
import qualified Network.Socket.ByteString as NBS
import Network.Socket.Internal
import qualified Network.Socket.ByteString.Lazy as SL
import System.IO

isSupportedFamily :: Family -> Bool
isSupportedFamily AF_INET = True
isSupportedFamily _       = False

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
                [(addr, "")] -> return (convertToWord32 addr)
                _            -> hostAddress `fmap` (getHostByName hostname)
     (port, proto) <- case msn of
                        Nothing -> return (0, defaultProtocol)
                        Just sn ->
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

getPeerName :: Socket -> IO SockAddr
getPeerName s =
  do nsock <- getConnectedHansSocket s ForNeither
     let oaddr = convertToWord32 (NS.sockRemoteHost nsock)
         oport = fromIntegral (getPort (NS.sockRemotePort nsock))
     return (SockAddrInet oport oaddr)

getSocketName :: Socket -> IO SockAddr
getSocketName s = 
  do nsock <- getConnectedHansSocket s ForNeither
     let lport = fromIntegral (getPort (NS.sockLocalPort nsock))
         laddr = convertToWord32 (IP4 127 0 0 1)
     return (SockAddrInet lport laddr)

getPeerCred :: Socket -> IO (CUInt, CUInt, CUInt)
getPeerCred _ =
  throwIO (userError "getPeerCred not supported on HaNS")

socketPort :: Socket -> IO PortNumber
socketPort sock =
  do SockAddrInet port _ <- getSocketName sock
     return port

socketToHandle :: Socket -> IOMode -> IO Handle
socketToHandle so mode =
  do nsock <- getConnectedHansSocket so ForBoth
     makeHansHandle nsock mode

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
        (\ (_ :: SomeException) -> return (show (convertFromWord32 addr)))

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

