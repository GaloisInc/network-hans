module Network.BSD(
         HostName
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
       , ProtocolEntry(..)
       , getProtocolByName
       , getProtocolByNumber
       , getProtocolNumber
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
       , ifNameToIndex
       )
 where

import Control.Exception(throw)
import Data.Typeable(Typeable)
import Foreign.C.Types(CInt, CULong)
import Hans.IP4(packIP4)
import Network.BSD.ServiceDB(ServiceEntry(..), ServiceName, serviceDB)
import Network.Socket.Internal(PortNumber(..), HostAddress, Family(..))
import Network.Socket.Types(fromIP4)

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

getServiceByName :: ServiceName -> ProtocolName -> IO ServiceEntry
getServiceByName sn pn = return (go serviceDB)
 where
  go [] = throw (userError "Service not found.")
  go (f:rest)
    | nameMatches f && (serviceProtocol f == pn) = f
    | otherwise                                  = go rest
  --
  nameMatches x = (serviceName x == sn) || (sn `elem` serviceAliases x)

getServiceByPort :: PortNumber -> ProtocolName -> IO ServiceEntry
getServiceByPort po pr = return (go serviceDB)
 where
  go [] = throw (userError "Service not found.")
  go (f:rest)
    | (servicePort f == po) && (serviceProtocol f == pr) = f
    | otherwise                                          = go rest

getServicePortNumber :: ServiceName -> IO PortNumber
getServicePortNumber sn = return (go serviceDB)
 where
  go [] = throw (userError "Service not found.")
  go (f:rest)
    | (serviceName f == sn) || (sn `elem` serviceAliases f) = servicePort f
    | otherwise                                             = go rest

getServiceEntries :: Bool -> IO [ServiceEntry]
getServiceEntries _ = return serviceDB

getServiceEntry :: IO ServiceEntry
getServiceEntry  = return (head serviceDB)

setServiceEntry :: Bool -> IO ()
setServiceEntry _ = return ()

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

networkDB :: [NetworkEntry]
networkDB  = [
    NetworkEntry "default"    [] AF_INET 0
  , NetworkEntry "loopback"   [] AF_INET (fromIP4' (packIP4 127 0 0 0))
  , NetworkEntry "link-local" [] AF_INET (fromIP4' (packIP4 169 254 0 0))
  ]
 where fromIP4' = fromIntegral . fromIP4

getNetworkByName :: NetworkName -> IO NetworkEntry
getNetworkByName nm = return (go networkDB)
 where
  go [] = throw (userError "Network not found")
  go (first:rest)
    | (networkName first == nm) || (nm `elem` networkAliases first) = first
    | otherwise                                                     = go rest

getNetworkByAddr :: NetworkAddr -> Family -> IO NetworkEntry
getNetworkByAddr addr fam = return (go networkDB)
 where
  go [] = throw (userError "Network not found")
  go (first:rest)
    | (networkFamily first == fam) && (networkAddress first == addr) = first
    | otherwise                                                      = go rest

getNetworkEntries :: Bool -> IO [NetworkEntry]
getNetworkEntries _ = return networkDB

setNetworkEntry :: Bool -> IO ()
setNetworkEntry _ = return ()

getNetworkEntry :: IO NetworkEntry
getNetworkEntry = return (head networkDB)

endNetworkEntry :: IO ()
endNetworkEntry = return ()

ifNameToIndex :: String -> IO (Maybe Int)
ifNameToIndex _ = return Nothing
