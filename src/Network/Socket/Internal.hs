module Network.Socket.Internal(
         HostAddress
       , HostAddress6
       , FlowInfo
       , ScopeID
       , PortNumber(..)
       , SockAddr(..)
       , peekSockAddr
       , pokeSockAddr
       , sizeOfSockAddr
       , sizeOfSockAddrByFamily
       , withSockAddr
       , withNewSockAddr
       , Family(..)
       , throwSocketError
       , throwSocketErrorCode
       , throwSocketErrorIfMinus1_
       , throwSocketErrorIfMinus1Retry
       , throwSocketErrorIfMinus1Retry_
       , throwSocketErrorIfMinus1RetryMayBlock
       , throwSocketErrorWaitRead
       , throwSocketErrorWaitWrite
       , withSocketsDo
       , zeroMemory
       )
 where

import Control.Exception(throwIO)
import Control.Monad(unless)
import Data.Typeable(Typeable)
import Data.Word(Word32, Word16)
import Foreign.C.Error(Errno(..), throwErrno, errnoToIOError,
                       throwErrnoIfMinus1_, throwErrnoIfMinus1Retry,
                       throwErrnoIfMinus1RetryMayBlock)
import Foreign.C.Types(CInt(..), CSize(..))
import Foreign.Marshal.Alloc(allocaBytes)
import Foreign.Ptr(Ptr, castPtr)
import Foreign.Storable(Storable(..))
import Network.Socket.Types(Socket)

type HostAddress = Word32

type HostAddress6 = (Word32, Word32, Word32, Word32)

type FlowInfo = Word32

type ScopeID = Word32

newtype PortNumber = PortNum Word16
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show, Typeable, Storable)

data SockAddr = SockAddrInet PortNumber HostAddress
 deriving (Eq, Ord, Show, Typeable)

peekSockAddr :: Ptr SockAddr -> IO SockAddr
peekSockAddr ptr =
  do family <- peek (castPtr ptr) :: IO Word16
     unless (family == 2) $
       throwIO (userError ("peekSockAddr: " ++ show family ++
                           "not supported on this platform."))
     addr <- peekByteOff (castPtr ptr) 4
     port <- peekByteOff (castPtr ptr) 2
     return (SockAddrInet (PortNum port) addr)

pokeSockAddr :: Ptr a -> SockAddr -> IO ()
pokeSockAddr ptr (SockAddrInet (PortNum port) addr) =
  do pokeByteOff (castPtr ptr) 0 (2 :: Word16)
     pokeByteOff (castPtr ptr) 2 port
     pokeByteOff (castPtr ptr) 4 addr

sizeOfSockAddr :: SockAddr -> Int
sizeOfSockAddr _ = 8

sizeOfSockAddrByFamily :: Family -> Int
sizeOfSockAddrByFamily f =
  case f of
    AF_INET -> 8
    _       -> error ("sizeOfSockAddrByFamily: " ++ show f ++
                      " not supported.")

withSockAddr :: SockAddr -> (Ptr SockAddr -> Int -> IO a) -> IO a
withSockAddr saddr action =
  allocaBytes (sizeOfSockAddr saddr) $ \ ptr ->
    do pokeSockAddr ptr saddr
       action (castPtr ptr) (sizeOfSockAddr saddr)

withNewSockAddr :: Family -> (Ptr SockAddr -> Int -> IO a) -> IO a
withNewSockAddr family action =
  allocaBytes (sizeOfSockAddrByFamily family) $ \ ptr ->
    action ptr (sizeOfSockAddrByFamily family)

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

throwSocketError :: String -> IO a
throwSocketError = throwErrno

throwSocketErrorCode :: String -> CInt ->IO a
throwSocketErrorCode loc errno =
  ioError (errnoToIOError loc (Errno errno) Nothing Nothing)

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@.  Discards the result of the
-- IO action after error handling.
throwSocketErrorIfMinus1_
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO ()
throwSocketErrorIfMinus1_ = throwErrnoIfMinus1_

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@, but retries in case of an
-- interrupted operation.
throwSocketErrorIfMinus1Retry
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO a
throwSocketErrorIfMinus1Retry = throwErrnoIfMinus1Retry

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@, but retries in case of an
-- interrupted operation. Discards the result of the IO action after
-- error handling.
throwSocketErrorIfMinus1Retry_
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO ()
throwSocketErrorIfMinus1Retry_ loc m =
    throwSocketErrorIfMinus1Retry loc m >> return ()

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@, but retries in case of an
-- interrupted operation.  Checks for operations that would block and
-- executes an alternative action before retrying in that case.
throwSocketErrorIfMinus1RetryMayBlock
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO b    -- ^ action to execute before retrying if an
               --   immediate retry would block
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO a
throwSocketErrorIfMinus1RetryMayBlock name on_block act =
    throwErrnoIfMinus1RetryMayBlock name act on_block

-- | Like 'throwSocketErrorIfMinus1Retry', but if the action fails with
-- @EWOULDBLOCK@ or similar, wait for the socket to be read-ready,
-- and try again.
throwSocketErrorWaitRead :: (Eq a, Num a) => Socket -> String -> IO a -> IO a
throwSocketErrorWaitRead _ _ _ =
  fail "FIXME: throwSocketErrorWaitRead is not supported in network-hans"

-- | Like 'throwSocketErrorIfMinus1Retry', but if the action fails with
-- @EWOULDBLOCK@ or similar, wait for the socket to be write-ready,
-- and try again.
throwSocketErrorWaitWrite :: (Eq a, Num a) => Socket -> String -> IO a -> IO a
throwSocketErrorWaitWrite _ _ _ =
  fail "FIXME: throwSocketErrorWaitWrite is not supported in network-hans"

withSocketsDo :: IO a -> IO a
withSocketsDo action = action

zeroMemory :: Ptr a -> CSize -> IO ()
zeroMemory p s = memset p 0 s

foreign import ccall unsafe "string.h"
  memset :: Ptr a -> CInt -> CSize -> IO ()
