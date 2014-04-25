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

import Control.Exception
import Data.Typeable
import Foreign.C.Types
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
setHostEntry _ = return ()

getHostEntry :: IO HostEntry
getHostEntry  = undefined

endHostEntry :: IO ()
endHostEntry  = return ()

data ServiceEntry = ServiceEntry {
    serviceName     :: ServiceName
  , serviceAliases  :: [ServiceName]
  , servicePort     :: PortNumber
  , serviceProtocol :: ProtocolName
  }
 deriving (Show, Typeable)

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
setServiceEntry _ = undefined

endServiceEntry :: IO ()
endServiceEntry  = return ()

type ProtocolName = String

type ProtocolNumber = CInt

data ProtocolEntry = ProtocolEntry {
    protoName    :: ProtocolName
  , protoAliases :: [ProtocolName]
  , protoNumber  :: ProtocolNumber
  }
 deriving (Read, Show, Typeable)

protocolDB :: [ProtocolEntry]
protocolDB = [
    ProtocolEntry "udp" ["UDP"] 17
  , ProtocolEntry "tcp" ["TCP"] 6
  ]

getProtocolByName :: ProtocolName -> IO ProtocolEntry
getProtocolByName nm = return (go protocolDB)
 where
  go [] = throw (userError "Protocol not found")
  go (first:rest)
    | (protoName first == nm) || (nm `elem` protoAliases first) = first
    | otherwise                                                 = go rest

getProtocolByNumber :: ProtocolNumber -> IO ProtocolEntry
getProtocolByNumber num = return (go protocolDB)
 where
  go [] = throw (userError "Protocol not found")
  go (first:rest)
    | protoNumber first == num = first
    | otherwise                = go rest

getProtocolNumber :: ProtocolName -> IO ProtocolNumber
getProtocolNumber name = protoNumber `fmap` (getProtocolByName name)

defaultProtocol :: ProtocolNumber
defaultProtocol = 0

getProtocolEntries :: Bool -> IO [ProtocolEntry]
getProtocolEntries _ = return protocolDB

setProtocolEntry :: Bool -> IO ()
setProtocolEntry _ = return ()

getProtocolEntry :: IO ProtocolEntry
getProtocolEntry = return (head protocolDB)

endProtocolEntry :: IO ()
endProtocolEntry = return ()

type NetworkName = String

type NetworkAddr = CULong

data NetworkEntry = NetworkEntry {
    networkName    :: NetworkName
  , networkAliases :: [NetworkName]
  , networkFamily  :: Family
  , networkAddress :: NetworkAddr
  }
 deriving (Read, Show, Typeable)

getNetworkByName :: NetworkName -> IO NetworkEntry
getNetworkByName  = undefined

getNetworkByAddr :: NetworkAddr -> Family -> IO NetworkEntry
getNetworkByAddr  = undefined

getNetworkEntries :: Bool -> IO [NetworkEntry]
getNetworkEntries  = undefined

setNetworkEntry :: Bool -> IO ()
setNetworkEntry _ = return ()

getNetworkEntry :: IO NetworkEntry
getNetworkEntry  = undefined

endNetworkEntry :: IO ()
endNetworkEntry = return ()
