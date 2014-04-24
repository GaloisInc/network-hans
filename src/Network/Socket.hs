module Network.Socket(
         Socket
       , Family(..)
       , isSupportedFamily
       , SocketType(..)
       , isSupportedSocketType
       , SockAddr(..)
       , SocketStatus(..)
       , ShutdownCmd(..)
       , ProtocolNumber
       , defaultProtocol
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

import Data.Typeable
import Data.Word
import Data.ByteString.Lazy.Char8(pack, unpack)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Network.Socket.Internal
import qualified Network.Socket.ByteString.Lazy as SL
import System.IO

isSupportedFamily :: Family -> Bool
isSupportedFamily  = undefined

data SocketType = NoSocketType
                | Stream
                | Datagram
  deriving (Eq, Ord, Read, Show, Typeable)

isSupportedSocketType :: SocketType -> Bool
isSupportedSocketType  = undefined

data SocketStatus = NotConnected
                  | Bound
                  | Listening
                  | Connected
                  | ConvertedToHandle
                  | Closed

data ShutdownCmd = ShutdownReceive | ShutdownSend | ShutdownBoth
  deriving (Typeable)

type ProtocolNumber = Int

defaultProtocol :: ProtocolNumber
defaultProtocol  = 0

type HostName = String

type ServiceName = String

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
defaultHints  = undefined

getAddrInfo :: Maybe AddrInfo -> Maybe HostName -> Maybe ServiceName ->
               IO [AddrInfo]
getAddrInfo  = undefined

data NameInfoFlag = NI_DGRAM | NI_NAMEREQD | NI_NOFQDN
                  | NI_NUMERICHOST | NI_NUMERICSERV
  deriving (Eq, Read, Show, Typeable)

getNameInfo :: [NameInfoFlag] -> Bool -> Bool -> SockAddr ->
               IO (Maybe HostName, Maybe ServiceName)
getNameInfo  = undefined

socket :: Family -> SocketType -> ProtocolNumber -> IO Socket
socket  = undefined

socketPair :: Family -> SocketType -> ProtocolNumber -> IO (Socket, Socket)
socketPair  = undefined

connect :: Socket -> SockAddr -> IO ()
connect  = undefined

bind :: Socket -> SockAddr -> IO ()
bind  = undefined

listen :: Socket -> Int -> IO ()
listen  = undefined

accept :: Socket -> IO (Socket, SockAddr)
accept  = undefined

getPeerName :: Socket -> IO SockAddr
getPeerName  = undefined

getSocketName :: Socket -> IO SockAddr
getSocketName  = undefined

getPeerCred :: Socket -> IO (CUInt, CUInt, CUInt)
getPeerCred  = undefined

socketPort :: Socket -> IO PortNumber
socketPort  = undefined

socketToHandle :: Socket -> IOMode -> IO Handle
socketToHandle = undefined

sendTo :: Socket -> String -> SockAddr -> IO Int
sendTo  = undefined

sendBufTo :: Socket -> Ptr a -> Int -> SockAddr -> IO Int
sendBufTo  = undefined

recvFrom :: Socket -> Int -> IO (String, Int, SockAddr)
recvFrom  = undefined

recvBufFrom :: Socket -> Ptr a -> Int -> IO (Int, SockAddr)
recvBufFrom  = undefined

send :: Socket -> String -> IO Int
send so st = fromIntegral `fmap` SL.send so (pack st)

recv :: Socket -> Int -> IO String
recv so sz = unpack `fmap` SL.recv so (fromIntegral sz)

recvLen :: Socket -> Int -> IO (String, Int)
recvLen  = undefined

sendBuf :: Socket -> Ptr Word8 -> Int -> IO Int
sendBuf  = undefined

recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf  = undefined

inet_addr :: String -> IO HostAddress
inet_addr  = undefined

inet_ntoa :: HostAddress -> IO String
inet_ntoa  = undefined

shutdown :: Socket -> ShutdownCmd -> IO ()
shutdown  = undefined

close :: Socket -> IO ()
close  = undefined

isConnected :: Socket -> IO Bool
isConnected  = undefined

isBound :: Socket -> IO Bool
isBound  = undefined

isListening :: Socket -> IO Bool
isListening  = undefined

isReadable :: Socket -> IO Bool
isReadable  = undefined

isWritable :: Socket -> IO Bool
isWritable  = undefined

data SocketOption = Debug | ReuseAddr | Type | SoError | DontRoute | Broadcast
                  | SendBuffer | RecvBuffer | KeepAlive | OOBInline | TimeToLive
                  | MaxSegment | NoDelay | Cork | Linger | ReusePort
                  | RecvLowWater | SendLowWater | RecvTimeOut | SendTimeOut
                  | UseLoopBack | IPv6Only | CustomSockOpt (CInt, CInt)

  deriving (Show, Typeable)

isSupportedSocketOption :: SocketOption -> Bool
isSupportedSocketOption  = undefined

getSocketOption :: Socket -> SocketOption -> IO Int
getSocketOption  = undefined

setSocketOption :: Socket -> SocketOption -> Int -> IO ()
setSocketOption  = undefined

aNY_PORT :: PortNumber
aNY_PORT  = undefined

iNADDR_ANY :: HostAddress
iNADDR_ANY  = undefined

iN6ADDR_ANY :: HostAddress6
iN6ADDR_ANY  = undefined

sOMAXCONN :: Int
sOMAXCONN  = undefined

sOL_SOCKET :: Int
sOL_SOCKET  = undefined

sCM_RIGHTS :: Int
sCM_RIGHTS  = undefined

maxListenQueue :: Int
maxListenQueue  = 128

withSocketsDo :: IO a -> IO a
withSocketsDo action = action

sClose :: Socket -> IO ()
sClose  = undefined
