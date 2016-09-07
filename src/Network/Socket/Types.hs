module Network.Socket.Types(
         Socket
       , newSocket
       , connectIP4
       , bindIP4
       , listen
       , acceptIP4
       , SocketStatus(..)
       , getSocketStatus
       , Direction(..)
       , directionOpen
       , withTcpSocket
       , withUdpSocket
       , socketToHandle
       , ShutdownCmd(..)
       , shutdown
       , close
       , toIP4, fromIP4
       , setNetworkStack
       )
 where

import Control.Concurrent.MVar(MVar, newMVar, withMVar,
                               readMVar, swapMVar,
                               modifyMVar, modifyMVar_)
import Control.Monad(void)
import Data.Bits(shiftL, shiftR, (.|.), (.&.))
import Data.Maybe(fromMaybe)
import Data.Typeable(Typeable)
import Data.Word(Word64, Word32)
import Hans.Addr(wildcardAddr)
import Hans.IP4(IP4, packIP4, unpackIP4)
import Hans.Lens(view)
import Hans.Socket(TcpSocket, TcpListenSocket, UdpSocket, SockPort,
                   tcpRemoteAddr, tcpRemotePort, defaultSocketConfig,
                   newUdpSocket, sConnect, sListen, sAccept, sClose)
import Hans.Socket.Handle(makeHansHandle)
import Hans.Types(NetworkStack)
import System.IO(Handle, IOMode(..))
import System.IO.Unsafe(unsafePerformIO)

data Socket = Socket (MVar SocketState) Word64

instance Eq Socket where
  (Socket _ a) == (Socket _ b) = a == b

newSocket :: Bool -> IO Socket
newSocket isTcp =
  do mv    <- newMVar (if isTcp then CreatedTcp else CreatedUdp)
     ident <- nextSocketIdent
     return (Socket mv ident)

data SocketState = CreatedTcp | CreatedUdp
                 | BoundUdp     (UdpSocket IP4)
                 | BoundTcp     (Maybe IP4)     (Maybe SockPort)
                 | ListeningTcp (TcpListenSocket IP4)
                 | ConnectedTcp (TcpSocket IP4) Direction
                 | Converted
                 | ClosedSocket

connectIP4 :: Socket -> IP4 -> SockPort -> IO ()
connectIP4 (Socket mvs _) addr port =
  modifyMVar_ mvs $ \ curstate ->
    case curstate of
      CreatedTcp ->
        do let conf = defaultSocketConfig
               src  = wildcardAddr undefined
           ns <- readMVar evilNetworkStackMVar
           sock <- sConnect ns conf Nothing src Nothing addr port
           return (ConnectedTcp sock ForBoth)
      CreatedUdp ->
        fail "Cannot connect UDP socket."
      BoundUdp _ ->
        fail "Cannot connect bound UDP socket."
      BoundTcp maddr mport ->
        do let conf = defaultSocketConfig
               src  = fromMaybe (wildcardAddr undefined) maddr
           ns <- readMVar evilNetworkStackMVar
           sock <- sConnect ns conf Nothing src mport addr port
           return (ConnectedTcp sock ForBoth)
      ListeningTcp _ ->
        fail "Canoot connect listening TCP socket."
      ConnectedTcp _ _ ->
        fail "Cannot connect connected TCP socket."
      Converted ->
        fail "Cannot connect converted socket."
      ClosedSocket ->
        fail "Cannot connect closed socket."

bindIP4 :: Socket -> Maybe IP4 -> Maybe SockPort -> IO ()
bindIP4 (Socket mvs _) maddr mport =
  modifyMVar_ mvs $ \ curstate ->
    case curstate of
      CreatedTcp ->
        return (BoundTcp maddr mport)
      CreatedUdp ->
        do let addr    = fromMaybe (wildcardAddr undefined) maddr
               conf    = defaultSocketConfig
           ns <- readMVar evilNetworkStackMVar
           sock <- newUdpSocket ns conf Nothing addr mport
           return (BoundUdp sock)
      BoundUdp _ ->
        fail "Cannot re-bind bound UDP port."
      BoundTcp _ _ ->
        fail "Cannot re-bind bound TCP port."
      ListeningTcp _ ->
        fail "Cannot bind listening TCP socket."
      ConnectedTcp _ _ ->
        fail "Cannot bind connected TCP socket."
      Converted ->
        fail "Cannot bind converted socket."
      ClosedSocket ->
        fail "Cannot connect closed socket."

listen :: Socket -> Int -> IO ()
listen (Socket mvs _) backlog =
  modifyMVar_ mvs $ \ curstate ->
    case curstate of
      BoundTcp maddr (Just port) ->
        do let addr = fromMaybe (wildcardAddr undefined) maddr
               conf = defaultSocketConfig
           ns <- readMVar evilNetworkStackMVar
           lsock <- sListen ns conf addr port backlog
           return (ListeningTcp lsock)
      BoundTcp _ Nothing ->
        fail "Cannot listen on socket with unbound port."
      _ ->
        fail "Cannot listen on unbound TCP port."

acceptIP4 :: Socket -> IO (Socket, IP4, SockPort)
acceptIP4 (Socket mvs _) =
  do curstate <- readMVar mvs
     case curstate of
       ListeningTcp lsock ->
         do sock <- sAccept lsock
            let addr = view tcpRemoteAddr sock
                port = view tcpRemotePort sock
            stateMV <- newMVar (ConnectedTcp sock ForBoth)
            ident   <- nextSocketIdent
            return (Socket stateMV ident, addr, port)
       _ ->
         fail "Illegal state for accept socket."

data ShutdownCmd = ShutdownReceive | ShutdownSend | ShutdownBoth
 deriving (Typeable, Eq)

shutdown :: Socket -> ShutdownCmd -> IO ()
shutdown (Socket mvs _) cmd =
  modifyMVar_ mvs $ \ curstate ->
    case curstate of
      ConnectedTcp sock _ | cmd == ShutdownBoth ->
        sClose sock >> return ClosedSocket
      ConnectedTcp sock ForRead | cmd == ShutdownReceive ->
        sClose sock >> return ClosedSocket
      ConnectedTcp sock ForWrite | cmd == ShutdownSend ->
        sClose sock >> return ClosedSocket
      ConnectedTcp sock _ | cmd == ShutdownReceive ->
        return (ConnectedTcp sock ForWrite)
      ConnectedTcp sock _ | cmd == ShutdownSend ->
        return (ConnectedTcp sock ForRead)
      ConnectedTcp _ _ ->
        fail "Internal consistency error in shutdown."
      _ ->
        fail "Shutdown called on un-connected socket."

close :: Socket -> IO ()
close (Socket mvs _) =
  modifyMVar_ mvs (\ _ -> return ClosedSocket)

socketToHandle :: Socket -> IOMode -> IO Handle
socketToHandle (Socket mvs _) mode =
  modifyMVar mvs $ \ curstate ->
    case curstate of
      ConnectedTcp sock ForBoth ->
        do hndl <- makeHansHandle sock mode
           return (Converted, hndl)
      ConnectedTcp sock ForRead | mode == ReadMode ->
        do hndl <- makeHansHandle sock mode
           return (Converted, hndl)
      ConnectedTcp sock ForWrite | mode `elem` [AppendMode, WriteMode] ->
        do hndl <- makeHansHandle sock mode
           return (Converted, hndl)
      ConnectedTcp _ allowed ->
        fail ("Access error converted socket to handle. Socket is in " ++
              show allowed ++ " mode, but IOMode was " ++ show mode)
      _ ->
        fail ("Cannot convert unconnected socket to a handle.")

data SocketStatus = NotConnected
                  | Bound
                  | Listening
                  | Connected
                  | ConvertedToHandle
                  | Closed
 deriving (Eq, Show)

getSocketStatus :: Socket -> IO SocketStatus
getSocketStatus (Socket mvs _) =
  withMVar mvs $ \ curstate ->
    case curstate of
      CreatedTcp       -> return NotConnected
      CreatedUdp       -> return NotConnected
      BoundUdp     _   -> return Bound
      BoundTcp     _ _ -> return Bound
      ListeningTcp _   -> return Listening
      ConnectedTcp _ _ -> return Connected
      Converted        -> return ConvertedToHandle
      ClosedSocket     -> return Closed

data Direction = ForWrite | ForRead | ForBoth | ForNeither
 deriving (Show)

directionOpen :: Socket -> Direction -> IO Bool
directionOpen (Socket mvs _) req =
  withMVar mvs $ \ curstate ->
    case curstate of
      ConnectedTcp _ dir -> return (modesMatch req dir)
      _                  -> return False

withTcpSocket :: Socket -> Direction -> (TcpSocket IP4 -> IO a) -> IO a
withTcpSocket (Socket mvs _) dir action =
  withMVar mvs $ \ curstate ->
    case curstate of
      ConnectedTcp sock dir' | modesMatch dir dir' ->
        action sock
      ConnectedTcp _ dir' ->
        fail ("Mismatch between requested direction (" ++ show dir ++
              ") and allowed (" ++ show dir' ++ ")")
      _ ->
        fail ("TCP operation on non-TCP socket.")

withUdpSocket :: Socket -> (UdpSocket IP4 -> IO a) -> IO a
withUdpSocket (Socket mvs _) action =
  withMVar mvs $ \ curstate ->
    case curstate of
      BoundUdp udps ->
        action udps
      _ ->
        fail "UDP operation on non-UDP socket."

toIP4 :: Word32 -> IP4
toIP4 w32 = packIP4 a b c d
 where
  a = fromIntegral ((w32 `shiftR` 24) .&. 0xFF)
  b = fromIntegral ((w32 `shiftR` 16) .&. 0xFF)
  c = fromIntegral ((w32 `shiftR`  8) .&. 0xFF)
  d = fromIntegral ((w32 `shiftR`  0) .&. 0xFF)

fromIP4 :: IP4 -> Word32
fromIP4 ipaddr = w32
 where
  (a, b, c, d) = unpackIP4 ipaddr
  --
  w32 = a' .|. b' .|. c' .|. d'
  a'  = fromIntegral a `shiftL` 24
  b'  = fromIntegral b `shiftL` 16
  c'  = fromIntegral c `shiftL`  8
  d'  = fromIntegral d `shiftL`  0
 
modesMatch :: Direction -> Direction -> Bool
modesMatch ForBoth    ForBoth  = True
modesMatch ForBoth    _        = False
modesMatch ForRead    ForBoth  = True
modesMatch ForRead    ForRead  = True
modesMatch ForRead    _        = False
modesMatch ForWrite   ForBoth  = True
modesMatch ForWrite   ForWrite = True
modesMatch ForWrite   _        = False
modesMatch ForNeither _        = True

{-# NOINLINE evilNetworkStackMVar #-}
evilNetworkStackMVar :: MVar NetworkStack
evilNetworkStackMVar =
  unsafePerformIO (newMVar (error "Access before network stack set!"))

{-# NOINLINE evilSocketIDMVar #-}
evilSocketIDMVar :: MVar Word64
evilSocketIDMVar =
  unsafePerformIO (newMVar 1)

setNetworkStack :: NetworkStack -> IO ()
setNetworkStack = void . swapMVar evilNetworkStackMVar

nextSocketIdent :: IO Word64
nextSocketIdent = modifyMVar evilSocketIDMVar (\ x -> return (x + 1, x))
