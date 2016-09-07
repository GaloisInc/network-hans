module Network.Socket.ByteString.Lazy(
         send
       , sendAll
       , getContents
       , recv
       )
 where

import           Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Int(Int64)
import           Hans.Socket(DataSocket(..))
import           Network.Socket.Types(Socket, Direction(..), withTcpSocket)
import           Prelude hiding (getContents)
import           System.IO.Unsafe(unsafeInterleaveIO)

send :: Socket -> ByteString -> IO Int64
send sock bstr =
  withTcpSocket sock ForWrite $ \ tcps ->
    fromIntegral `fmap` sWrite tcps bstr

sendAll :: Socket -> ByteString -> IO ()
sendAll sock bstr
  | L.length bstr == 0 = return ()
  | otherwise          =
      do sent <- send sock bstr
         sendAll sock (L.drop sent bstr)

getContents :: Socket -> IO ByteString
getContents socket = lazyRead
 where
  lazyRead = unsafeInterleaveIO loop
  loop =
    do bstr <- recv socket 4096
       if L.null bstr
          then return L.empty
          else do next <- lazyRead
                  return (bstr `L.append` next)

recv :: Socket -> Int64 -> IO ByteString
recv sock amt =
  withTcpSocket sock ForRead $ \ tcps ->
    sRead tcps (fromIntegral amt)

