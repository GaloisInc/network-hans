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

import           Data.ByteString(ByteString)
import qualified Data.ByteString                as S
import qualified Data.ByteString.Lazy           as L
import           Hans.Socket(recvfrom, sendto)
import qualified Network.Socket.ByteString.Lazy as NSL
import           Network.Socket.Internal(SockAddr(..), PortNumber(..))
import           Network.Socket.Types(Socket, withUdpSocket, toIP4, fromIP4)

send :: Socket -> ByteString -> IO Int
send sock bstr = fromIntegral `fmap` NSL.send sock (L.fromStrict bstr)

sendAll :: Socket -> ByteString -> IO ()
sendAll sock bstr = NSL.sendAll sock (L.fromStrict bstr)

sendTo :: Socket -> ByteString -> SockAddr -> IO Int
sendTo s b a =
  do sendAllTo s b a
     return (S.length b)

sendAllTo :: Socket -> ByteString -> SockAddr -> IO ()
sendAllTo sock bstr (SockAddrInet (PortNum port) addr) =
  withUdpSocket sock $ \ usock ->
    sendto usock addr' port' bstr'
 where
  addr' = toIP4 addr
  port' = fromIntegral port
  bstr' = L.fromStrict bstr

sendMany :: Socket -> [ByteString] -> IO ()
sendMany sock bstrs = NSL.sendAll sock (L.fromChunks bstrs)

sendManyTo :: Socket -> [ByteString] -> SockAddr -> IO ()
sendManyTo sock bstrs (SockAddrInet (PortNum port) addr) =
  withUdpSocket sock $ \ usock ->
   sendto usock addr' port' bstr'
 where
  addr' = toIP4 addr
  port' = fromIntegral port
  bstr' = L.fromChunks bstrs

recv :: Socket -> Int -> IO ByteString
recv sock amt = L.toStrict `fmap` NSL.recv sock (fromIntegral amt)

recvFrom :: Socket -> Int -> IO (ByteString, SockAddr)
recvFrom sock maxbytes =
  withUdpSocket sock $ \ usock ->
    do (_, addr, port, inbstr) <- recvfrom usock
       let res = L.toStrict (L.take (fromIntegral maxbytes) inbstr)
       return (res, SockAddrInet (PortNum port) (fromIP4 addr))

