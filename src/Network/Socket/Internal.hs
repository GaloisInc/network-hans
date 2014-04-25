module Network.Socket.Internal(
         Socket(..)
       , SocketStatus(..)
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
       )
 where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Data.Typeable
import Data.ByteString(ByteString)
import Data.Word(Word64, Word32,Word16)
import Foreign.Storable
import Hans.Address.IP4
import Hans.NetworkStack(NetworkStack)
import qualified Hans.NetworkStack as NS
import System.IO.Unsafe

data Socket = Socket {
    sockState  :: MVar SockState
  , sockNStack :: NetworkStack
  , sockIdent  :: Word64
  }

getConnectedHansSocket :: Socket -> IO NS.Socket
getConnectedHansSocket sock =
  do state <- readMVar (sockState sock)
     case state of
       SockConnected s -> return s
       _               -> throw (userError "Socket not in connected state.")

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

data SockState = SockInitial
               | SockConnected NS.Socket
               | SockBoundUdp (Maybe NS.UdpPort) (Chan (ByteString, SockAddr))

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

