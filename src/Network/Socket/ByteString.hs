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
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Hans.NetworkStack(sendUdp)
import Network.Socket.Internal
import qualified Network.Socket.ByteString.Lazy as NSL

send :: Socket -> ByteString -> IO Int
send sock bstr = fromIntegral `fmap` NSL.send sock (L.fromStrict bstr)

sendAll :: Socket -> ByteString -> IO ()
sendAll sock bstr = NSL.sendAll sock (L.fromStrict bstr)

sendTo :: Socket -> ByteString -> SockAddr -> IO Int
sendTo sock bstr saddr =
  do (ns, myport) <- getBoundUdpPort sock
     sendUdp ns dest myport dport (L.fromStrict bstr)
     return (fromIntegral (S.length bstr))
 where (dest, dport) = hansUdpSockAddr saddr

sendAllTo :: Socket -> ByteString -> SockAddr -> IO ()
sendAllTo s b a = sendTo s b a >> return ()

sendMany :: Socket -> [ByteString] -> IO ()
sendMany sock bstrs = NSL.sendAll sock (L.fromChunks bstrs)

sendManyTo :: Socket -> [ByteString] -> SockAddr -> IO ()
sendManyTo sock bstrs saddr =
  do (ns, myport) <- getBoundUdpPort sock
     sendUdp ns dest myport dport (L.fromChunks bstrs)
 where (dest, dport) = hansUdpSockAddr saddr

recv :: Socket -> Int -> IO ByteString
recv sock amt = L.toStrict `fmap` NSL.recv sock (fromIntegral amt)

recvFrom :: Socket -> Int -> IO (ByteString, SockAddr)
recvFrom sock maxbytes =
  do (bstr, addr) <- getNextUdpPacket sock
     return (S.take (fromIntegral maxbytes) bstr, addr)

