module Network.Socket.Internal(
         Socket(..)
       , SocketStatus(..)
       , socket
       , connect
       , bind
       , listen
       , accept
       , ShutdownCmd(..)
       , close
       , shutdown
       , isConnected
       , isBound
       , isListening
       , isReadable
       , isWritable
       , SocketType(..)
       , isSupportedSocketType
       , ProtocolNumber
       , defaultProtocol
       , ForAction(..)
       , getConnectedHansSocket
       , getBoundUdpPort
       , getNextUdpPacket
       , SockAddr(..)
       , hansUdpSockAddr, hansTcpSockAddr
       , PortNumber(..)
       , Family(..)
       , HostAddress
       , HostAddress6
       , getNetworkHansStack
       , setNetworkHansStack
       , getNextSockIdent
       )
 where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Typeable
import Data.ByteString(ByteString)
import Data.Word(Word64, Word32,Word16)
import Foreign.C.Types
import Foreign.Storable
import Hans.Address.IP4
import Hans.Message.Tcp(getPort)
import Hans.Message.Udp(getUdpPort)
import Hans.NetworkStack(NetworkStack)
import qualified Hans.NetworkStack as NS
import System.IO.Unsafe

type ProtocolNumber = CInt

defaultProtocol :: ProtocolNumber
defaultProtocol = 6

data Socket = Socket {
    sockState  :: MVar SockState
  , sockNStack :: NetworkStack
  , sockIdent  :: Word64
  }

data ForAction = ForNeither | ForRead | ForWrite | ForBoth
  deriving (Eq)

getConnectedHansSocket :: Socket -> ForAction -> IO NS.Socket
getConnectedHansSocket sock forWrite =
  do state <- readMVar (sockState sock)
     case state of
       SockConnected s c -> checkDone c forWrite >> return s
       _                 -> throw (userError "Socket not in connected state.")
 where
  checkDone Nothing _ =
           return ()
  checkDone (Just ShutdownReceive) x
    | x `elem` [ForRead, ForBoth] =
           throw (userError "Read on Socket set shutdown/receive.")
    | otherwise =
           return ()
  checkDone (Just ShutdownSend) x
    | x `elem` [ForWrite, ForBoth] =
           throw (userError "Write on Socket set shutdown/send.")
    | otherwise =
           return ()
  checkDone (Just ShutdownBoth) x
    | x /= ForNeither =
           throw (userError "Read or write on shutdown socket.")
    | otherwise =
           return ()

getBoundUdpPort :: Socket -> IO (NetworkStack, Maybe NS.UdpPort)
getBoundUdpPort sock =
  do state <- readMVar (sockState sock)
     case state of
       SockBoundUdp mp _ -> return (sockNStack sock, mp)
       _                 -> throw (userError "Socket not bound to UDP.")

getNextUdpPacket :: Socket -> IO (ByteString, SockAddr)
getNextUdpPacket sock =
  do state <- readMVar (sockState sock)
     case state of
       SockBoundUdp _ c -> readChan c
       _                -> throw (userError "Socket not bound to UDP.")

instance Eq Socket where
  a == b = sockIdent a == sockIdent b

data SocketStatus = NotConnected
                  | Bound
                  | Listening
                  | Connected
                  | ConvertedToHandle
                  | Closed
 deriving (Eq, Show)

data SocketType = NoSocketType
                | Stream
                | Datagram
                | Raw
                | RDM
                | SeqPacket
  deriving (Eq, Ord, Read, Show, Typeable)

isSupportedSocketType :: SocketType -> Bool
isSupportedSocketType Stream   = True
isSupportedSocketType Datagram = True

data SockState = SockInitialUdp
               | SockInitialTcp
               | SockConnected NS.Socket (Maybe ShutdownCmd)
               | SockBoundUdp (Maybe NS.UdpPort) (Chan (ByteString, SockAddr))
               | SockBoundTcp IP4 NS.TcpPort
               | SockListening NS.Socket
               | SockClosed

instance Eq SockState where
  SockInitialTcp == SockInitialTcp = True
  SockInitialUdp == SockInitialUdp = True
  SockClosed     == SockClosed     = True
  _              == _              = False

socket :: Family -> SocketType -> ProtocolNumber -> IO Socket
socket AF_INET Stream   6  =
  do sockState  <- newMVar SockInitialTcp
     sockNStack <- getNetworkHansStack
     sockIdent  <- getNextSockIdent
     return Socket { .. }
socket AF_INET Datagram 17 =
  do sockState  <- newMVar SockInitialUdp
     sockNStack <- getNetworkHansStack
     sockIdent  <- getNextSockIdent
     return Socket { .. }
socket _       _        _  =
  throw (userError "ERROR: Unsupported socket options.")

connect :: Socket -> SockAddr -> IO ()
connect sock (SockAddrInet (PortNum port) addr) =
  do state <- takeMVar (sockState sock)
     unless (state == SockInitialTcp) $
       throw (userError "Attempt to connect() on a non-TCP socket.")
     let dest  = convertFromWord32 addr
         dport = fromIntegral port
         ns    = sockNStack sock
     catch (do hsock <- NS.connect ns dest dport Nothing
               putMVar (sockState sock) (SockConnected hsock Nothing))
           (\ e ->
             do putMVar (sockState sock) state
                throw (e :: SomeException))

bind :: Socket -> SockAddr -> IO ()
bind sock (SockAddrInet (PortNum port) addr) =
  do state <- takeMVar (sockState sock)
     case state of
       SockInitialUdp ->
         do chan <- newChan
            let port' = fromIntegral port
            NS.addUdpHandler (sockNStack sock) port' (udpHandler chan)
            let state' = SockBoundUdp (Just port') chan
            putMVar (sockState sock) state'
       SockInitialTcp ->
         do let port' = fromIntegral port
                addr' = convertFromWord32 addr
            putMVar (sockState sock) (SockBoundTcp addr' port')
       _ ->
         do putMVar (sockState sock) state
            throw (userError "Bind called on incompatible socket.")
 where
  udpHandler :: Chan (ByteString, SockAddr) -> IP4 -> NS.UdpPort -> ByteString -> IO ()
  udpHandler chan addr port bstr =
    let sport = PortNum (fromIntegral (getUdpPort port))
        saddr = convertToWord32 addr
    in writeChan chan (bstr, SockAddrInet sport saddr)

listen :: Socket -> Int -> IO ()
listen sock _ =
  do state <- takeMVar (sockState sock)
     case state of
       SockBoundTcp addr port ->
         do nsock <- NS.listen (sockNStack sock) addr port
            putMVar (sockState sock) (SockListening nsock)
       _ ->
         throw (userError "Listen called on unbound or non-TCP socket.")

accept :: Socket -> IO (Socket, SockAddr)
accept sock =
  do state <- readMVar (sockState sock)
     case state of
       SockListening lsock ->
         do newsock <- NS.accept lsock
            state   <- newMVar (SockConnected newsock Nothing)
            newid   <- getNextSockIdent
            let sock' = Socket state (sockNStack sock) newid
                oaddr = convertToWord32 (NS.sockRemoteHost newsock)
                oport = fromIntegral (getPort (NS.sockRemotePort newsock))
            return (sock', SockAddrInet oport oaddr)

data ShutdownCmd = ShutdownReceive | ShutdownSend | ShutdownBoth
  deriving (Typeable, Eq)

shutdown :: Socket -> ShutdownCmd -> IO ()
shutdown so cmd =
  do state <- takeMVar (sockState so)
     case state of
       SockConnected sock oshtd ->
         do let shtd = advanceShutdown oshtd cmd
            if shtd == ShutdownBoth
               then do putMVar (sockState so) SockClosed
                       NS.close sock
               else putMVar (sockState so) (SockConnected sock (Just shtd))

advanceShutdown :: Maybe ShutdownCmd -> ShutdownCmd -> ShutdownCmd
advanceShutdown Nothing                x               = x
advanceShutdown _                      ShutdownBoth    = ShutdownBoth
advanceShutdown (Just ShutdownReceive) ShutdownSend    = ShutdownBoth
advanceShutdown (Just ShutdownSend)    ShutdownReceive = ShutdownBoth
advanceShutdown (Just y)               x               = y

close :: Socket -> IO ()
close so =
  do state <- takeMVar (sockState so)
     case state of
       SockConnected sock       _ -> NS.close sock
       SockBoundUdp (Just port) _ -> NS.removeUdpHandler (sockNStack so) port
       SockListening sock         -> NS.close sock
       _                          -> return ()
     putMVar (sockState so) SockClosed

isConnected :: Socket -> IO Bool
isConnected so =
  do state <- readMVar (sockState so)
     case state of
       SockConnected _ _ -> return True
       _                 -> return False

isBound :: Socket -> IO Bool
isBound so =
  do state <- readMVar (sockState so)
     case state of
       SockBoundUdp _ _ -> return True
       SockBoundTcp _ _ -> return True
       _                -> return False

isListening :: Socket -> IO Bool
isListening so =
  do state <- readMVar (sockState so)
     case state of
       SockListening _ -> return True
       _               -> return False

isReadable :: Socket -> IO Bool
isReadable so =
  do state <- readMVar (sockState so)
     case state of
       SockBoundUdp _ _                       -> return True
       SockConnected _ (Just ShutdownReceive) -> return False
       SockConnected _ _                      -> return  True
       _                                      -> return False

isWritable :: Socket -> IO Bool
isWritable so = 
  do state <- readMVar (sockState so)
     case state of
       SockBoundUdp _ _                    -> return True
       SockConnected _ (Just ShutdownSend) -> return False
       SockConnected _ _                   -> return  True
       _                                   -> return False

data SockAddr = SockAddrInet PortNumber HostAddress
 deriving (Eq, Ord, Show, Typeable)

hansUdpSockAddr :: SockAddr -> (IP4, NS.UdpPort)
hansUdpSockAddr (SockAddrInet (PortNum pn) addr) =
  (convertFromWord32 addr, fromIntegral pn)

hansTcpSockAddr :: SockAddr -> (IP4, NS.TcpPort)
hansTcpSockAddr (SockAddrInet (PortNum pn) addr) =
  (convertFromWord32 addr, fromIntegral pn)

newtype PortNumber = PortNum Word16
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show, Typeable, Storable)

data Family
    = AF_UNSPEC           -- ^unspecified
    | AF_UNIX             -- ^local to host (pipes, portals
    | AF_INET             -- ^internetwork: UDP, TCP, etc
    | AF_INET6            -- ^Internet Protocol version 6
    | AF_IMPLINK          -- ^arpanet imp addresses
    | AF_PUP              -- ^pup protocols: e.g. BSP
    | AF_CHAOS            -- ^mit CHAOS protocols
    | AF_NS               -- ^XEROX NS protocols
    | AF_NBS              -- ^nbs protocols
    | AF_ECMA             -- ^european computer manufacturers
    | AF_DATAKIT          -- ^datakit protocols
    | AF_CCITT            -- ^CCITT protocols, X.25 etc
    | AF_SNA              -- ^IBM SNA
    | AF_DECnet           -- ^DECnet
    | AF_DLI              -- ^Direct data link interface
    | AF_LAT              -- ^LAT
    | AF_HYLINK           -- ^NSC Hyperchannel
    | AF_APPLETALK        -- ^Apple Talk
    | AF_ROUTE            -- ^Internal Routing Protocol
    | AF_NETBIOS          -- ^NetBios-style addresses
    | AF_NIT              -- ^Network Interface Tap
    | AF_802              -- ^IEEE 802.2, also ISO 8802
    | AF_ISO              -- ^ISO protocols
    | AF_OSI              -- ^umbrella of all families used by OSI
    | AF_NETMAN           -- ^DNA Network Management
    | AF_X25              -- ^CCITT X.25
    | AF_AX25
    | AF_OSINET           -- ^AFI
    | AF_GOSSIP           -- ^US Government OSI
    | AF_IPX              -- ^Novell Internet Protocol
    | Pseudo_AF_XTP       -- ^eXpress Transfer Protocol (no AF)
    | AF_CTF              -- ^Common Trace Facility
    | AF_WAN              -- ^Wide Area Network protocols
    | AF_SDL              -- ^SGI Data Link for DLPI
    | AF_NETWARE
    | AF_NDD
    | AF_INTF             -- ^Debugging use only
    | AF_COIP             -- ^connection-oriented IP, aka ST II
    | AF_CNT              -- ^Computer Network Technology
    | Pseudo_AF_RTIP      -- ^Help Identify RTIP packets
    | Pseudo_AF_PIP       -- ^Help Identify PIP packets
    | AF_SIP              -- ^Simple Internet Protocol
    | AF_ISDN             -- ^Integrated Services Digital Network
    | Pseudo_AF_KEY       -- ^Internal key-management function
    | AF_NATM             -- ^native ATM access
    | AF_ARP              -- ^(rev.) addr. res. prot. (RFC 826)
    | Pseudo_AF_HDRCMPLT  -- ^Used by BPF to not rewrite hdrs in iface output
    | AF_ENCAP
    | AF_LINK             -- ^Link layer interface
    | AF_RAW              -- ^Link layer interface
    | AF_RIF              -- ^raw interface
    | AF_NETROM           -- ^Amateur radio NetROM
    | AF_BRIDGE           -- ^multiprotocol bridge
    | AF_ATMPVC           -- ^ATM PVCs
    | AF_ROSE             -- ^Amateur Radio X.25 PLP
    | AF_NETBEUI          -- ^802.2LLC
    | AF_SECURITY         -- ^Security callback pseudo AF
    | AF_PACKET           -- ^Packet family
    | AF_ASH              -- ^Ash
    | AF_ECONET           -- ^Acorn Econet
    | AF_ATMSVC           -- ^ATM SVCs
    | AF_IRDA             -- ^IRDA sockets
    | AF_PPPOX            -- ^PPPoX sockets
    | AF_WANPIPE          -- ^Wanpipe API sockets
    | AF_BLUETOOTH        -- ^bluetooth sockets
  deriving (Eq, Ord, Read, Show)

type HostAddress = Word32

type HostAddress6 = (Word32, Word32, Word32, Word32)

{-# NOINLINE evilNSMVar #-}
evilNSMVar :: MVar NetworkStack
evilNSMVar =
  unsafePerformIO (newMVar (error "Access before set of network stack!"))

getNetworkHansStack :: IO NetworkStack
getNetworkHansStack = readMVar evilNSMVar

setNetworkHansStack :: NetworkStack -> IO ()
setNetworkHansStack ns = swapMVar evilNSMVar ns >> return ()

{-# NOINLINE evilSockIdentMVar #-}
evilSockIdentMVar :: MVar Word64
evilSockIdentMVar = unsafePerformIO (newMVar 0)

getNextSockIdent :: IO Word64
getNextSockIdent  = modifyMVar evilSockIdentMVar (\ x -> return (x + 1, x))

