module Network.Socket.ByteString.Lazy(
         send
       , sendAll
       , getContents
       , recv
       )
 where

import Data.ByteString.Lazy(ByteString)
import Data.Int
import Network.Socket.Internal
import Prelude hiding (getContents)

send :: Socket -> ByteString -> IO Int64
send  = undefined

sendAll :: Socket -> ByteString -> IO ()
sendAll  = undefined

getContents :: Socket -> IO ByteString
getContents  = undefined

recv :: Socket -> Int64 -> IO ByteString
recv  = undefined
