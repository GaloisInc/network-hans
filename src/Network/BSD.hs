module Network.BSD(
         HostName(..)
       , getHostName
       , HostEntry(..)
       , getHostByName
       , getHostByAddr
       , hostAddress
       , getHostEntries
       , setHostEntry
       , getHostEntry
       , endHostEntry
       , ServiceEntry(..)
       , ServiceName
       , getServiceByName
       , getServiceByPort
       , getServicePortNumber
       , getServiceEntries
       , getServiceEntry
       , setServiceEntry
       , endServiceEntry
       , ProtocolName
       , ProtocolNumber
       , ProtocolEntry
       , getProtocolByName
       , getProtocolByNumber
       , getProtocolNumber
       , defaultProtocol
       , getProtocolEntries
       , setProtocolEntry
       , getProtocolEntry
       , endProtocolEntry
       , PortNumber(..)
       , NetworkAddr
       , NetworkEntry(..)
       , getNetworkByName
       , getNetworkByAddr
       , getNetworkEntries
       , setNetworkEntry
       , getNetworkEntry
       , endNetworkEntry
       )
 where

import Data.Typeable
import Foreign.C.Types
import Foreign.Storable
import Network.Socket.Internal

type HostName = String

getHostName :: IO String
getHostName  = undefined

data HostEntry = HostEntry {
    hostName      :: HostName
  , hostAliases   :: [HostName]
  , hostFamily    :: Family
  , hostAddresses :: [HostAddress]
  }
 deriving (Read, Show, Typeable)

getHostByName :: HostName -> IO HostEntry
getHostByName  = undefined

getHostByAddr :: Family -> HostAddress -> IO HostEntry
getHostByAddr  = undefined

hostAddress :: HostEntry -> HostAddress
hostAddress  = undefined

getHostEntries :: Bool -> IO [HostEntry]
getHostEntries  = undefined

setHostEntry :: Bool -> IO ()
setHostEntry  = undefined

getHostEntry :: IO HostEntry
getHostEntry  = undefined

endHostEntry :: IO ()
endHostEntry  = undefined

data ServiceEntry = ServiceEntry {
    serviceName     :: ServiceName
  , serviceAliases  :: [ServiceName]
  , servicePort     :: PortNumber
  , serviceProtocol :: ProtocolName
  }
 deriving (Show, Typeable)

instance Storable ServiceEntry where
  sizeOf _ = undefined
  alignment _ = undefined
  peek _ = undefined
  poke _ = undefined

type ServiceName = String

getServiceByName :: ServiceName -> ProtocolName -> IO ServiceEntry
getServiceByName  = undefined

getServiceByPort :: PortNumber -> ProtocolName -> IO ServiceEntry
getServiceByPort  = undefined

getServicePortNumber :: ServiceName -> IO PortNumber
getServicePortNumber  = undefined

getServiceEntries :: Bool -> IO [ServiceEntry]
getServiceEntries  = undefined

getServiceEntry :: IO ServiceEntry
getServiceEntry  = undefined

setServiceEntry :: Bool -> IO ()
setServiceEntry  = undefined

endServiceEntry :: IO ()
endServiceEntry  = undefined

type ProtocolName = String

type ProtocolNumber = CInt

data ProtocolEntry = ProtocolEntry {
    protoName    :: ProtocolName
  , protoAliases :: [ProtocolName]
  , protoNumber  :: ProtocolNumber
  }
 deriving (Read, Show, Typeable)

instance Storable ProtocolEntry where
  sizeOf _ = undefined
  alignment _ = undefined
  peek _ = undefined
  poke _ = undefined

getProtocolByName :: ProtocolName -> IO ProtocolEntry
getProtocolByName  = undefined

getProtocolByNumber :: ProtocolNumber -> IO ProtocolEntry
getProtocolByNumber  = undefined

getProtocolNumber :: ProtocolName -> IO ProtocolNumber
getProtocolNumber  = undefined

defaultProtocol :: ProtocolNumber
defaultProtocol  = undefined

getProtocolEntries :: Bool -> IO [ProtocolEntry]
getProtocolEntries  = undefined

setProtocolEntry :: Bool -> IO ()
setProtocolEntry  = undefined

getProtocolEntry :: IO ProtocolEntry
getProtocolEntry  = undefined

endProtocolEntry :: IO ()
endProtocolEntry  = undefined

type NetworkName = String

type NetworkAddr = CULong

data NetworkEntry = NetworkEntry {
    networkName    :: NetworkName
  , networkAliases :: [NetworkName]
  , networkFamily  :: Family
  , networkAddress :: NetworkAddr
  }
 deriving (Read, Show, Typeable)

instance Storable NetworkEntry where
  sizeOf _ = undefined
  alignment _ = undefined
  peek _ = undefined
  poke _ = undefined

getNetworkByName :: NetworkName -> IO NetworkEntry
getNetworkByName  = undefined

getNetworkByAddr :: NetworkAddr -> Family -> IO NetworkEntry
getNetworkByAddr  = undefined

getNetworkEntries :: Bool -> IO [NetworkEntry]
getNetworkEntries  = undefined

setNetworkEntry :: Bool -> IO ()
setNetworkEntry  = undefined

getNetworkEntry :: IO NetworkEntry
getNetworkEntry  = undefined

endNetworkEntry :: IO ()
endNetworkEntry  = undefined
