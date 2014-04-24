module Network.Socket.ByteString(
         send
       , sendAll
       , sendTo
       , sendAllTo
       , sendMany
       , sendManyTo
       , recv
       , recvFrom
       )
 where

import Data.ByteString(ByteString)
import Network.Socket.Internal

send :: Socket -> ByteString -> IO Int
send  = undefined

sendAll :: Socket -> ByteString -> IO ()
sendAll  = undefined

sendTo :: Socket -> ByteString -> SockAddr -> IO Int
sendTo  = undefined

sendAllTo :: Socket -> ByteString -> SockAddr -> IO ()
sendAllTo  = undefined

sendMany :: Socket -> [ByteString] -> IO ()
sendMany  = undefined

sendManyTo :: Socket -> [ByteString] -> SockAddr -> IO ()
sendManyTo  = undefined

recv :: Socket -> Int -> IO ByteString
recv  = undefined

recvFrom :: Socket -> Int -> IO (ByteString, SockAddr)
recvFrom  = undefined


