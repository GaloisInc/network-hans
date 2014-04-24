module Network.Socket.Internal(
         Socket(..)
       , SockAddr(..)
       , PortNumber(..)
       , Family(..)
       , HostAddress
       , HostAddress6
       , getNetworkHansStack
       , setNetworkHansStack
       )
 where

import Data.Typeable
import Data.Word(Word32,Word16)
import Foreign.Storable
import Hans.NetworkStack(NetworkStack)

data Socket = Socket
  deriving (Eq)

data SockAddr = SockAddrInet PortNumber HostAddress
 deriving (Eq, Ord, Show, Typeable)

newtype PortNumber = PortNum Word16
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show, Typeable, Storable)

data Family = AF_INET
  deriving (Eq, Ord, Read, Show)

type HostAddress = Word32

type HostAddress6 = (Word32, Word32, Word32, Word32)

getNetworkHansStack :: IO NetworkStack
getNetworkHansStack  = undefined

setNetworkHansStack :: NetworkStack -> IO ()
setNetworkHansStack  = undefined
