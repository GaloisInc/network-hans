module Hans.Socket.Handle(makeHansHandle) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BSS
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Typeable
import Foreign.Ptr
import GHC.IO.Buffer
import GHC.IO.BufferedIO
import GHC.IO.Device
import GHC.IO.Handle
import Hans.Layer.Tcp.Socket(canSend, canRecv)
import Hans.NetworkStack
import Prelude hiding (read)
import System.IO

data BufferedSocket = BS {
    bsSocket   :: Socket
  , bsRefCount :: MVar Int
  }
 deriving (Typeable)

newBufferedSocket :: Socket -> IO BufferedSocket
newBufferedSocket sock =
  do mv <- newMVar 1
     return (BS sock mv)

addRef :: BufferedSocket -> IO ()
addRef bs = modifyMVar_ (bsRefCount bs) (return . (+ 1))

dropRef :: BufferedSocket -> IO ()
dropRef bs =
  do count <- modifyMVar (bsRefCount bs) $
                \ x ->
                  let x' = x - 1
                  in return (x', x')
     when (count == 0) $ Hans.NetworkStack.close (bsSocket bs)

instance IODevice BufferedSocket where
  ready dev write msecs =
    do let tester = if write then canSend else canRecv
       canDo <- tester (bsSocket dev)
       if | canDo      -> return True
          | msecs <= 0 -> return False
          | otherwise  -> do let delay = min msecs 100
                             threadDelay (delay * 1000)
                             ready dev write (msecs - delay)
  close bs     = dropRef bs
  isTerminal _ = return False
  isSeekable _ = return False
  seek _ _ _   = throw (userError "Seek on HaNS socket.")
  tell _       = throw (userError "Tell on HaNS socket.")
  getSize _    = throw (userError "getSize on HaNS socket.")
  setSize _ _  = throw (userError "setSize on HaNS socket.")
  setEcho _ _  = throw (userError "setEcho on HaNS socket.")
  getEcho _    = throw (userError "getEcho on HaNS socket.")
  setRaw _ _   = return ()
  devType _    = return Stream
  dup bs       = addRef bs >> return bs
  dup2 _ _     = throw (userError "dup2 on HaNS socket.")

instance RawIO BufferedSocket where
  read sock dptr sz =
    do bstr <- recvBytes (bsSocket sock) (fromIntegral sz)
       when (BS.length bstr > 0) $ copyBS (BS.toChunks bstr) dptr sz
       return (fromIntegral (BS.length bstr))
  readNonBlocking sock dptr sz =
    do canGo <- canRecv (bsSocket sock)
       if canGo
          then Just `fmap` read sock dptr sz
          else return (Just 0)
  write sock ptr sz =
    do bstr <- BSS.packCStringLen (castPtr ptr, sz)
       sendAll (bsSocket sock) (BS.fromStrict bstr)
   where
    sendAll sock bstr
      | BS.null bstr = return ()
      | otherwise    = do num <- sendBytes sock bstr
                          sendAll sock (BS.drop (fromIntegral num) bstr)
  writeNonBlocking sock ptr sz =
    do canGo <- canSend (bsSocket sock)
       if canGo
          then do bstr <- BSS.packCStringLen (castPtr ptr, sz)
                  num  <- sendBytes (bsSocket sock) (BS.fromStrict bstr)
                  return (fromIntegral num)
          else return 0

instance BufferedIO BufferedSocket where
  newBuffer         _ = newByteBuffer (64 * 1024)
  fillReadBuffer      = readBuf
  fillReadBuffer0     = readBufNonBlocking
  flushWriteBuffer    = writeBuf
  flushWriteBuffer0   = writeBufNonBlocking

makeHansHandle :: Socket -> IOMode -> IO Handle
makeHansHandle socket mode =
  do buffSocket <- newBufferedSocket socket
     mkFileHandle buffSocket "<socket>" mode Nothing noNewlineTranslation

copyBS :: [BSS.ByteString] -> Ptr a -> Int -> IO ()
copyBS [] _ _ = return ()
copyBS (f:rest) sptr szLeft
  | BSS.null f   = copyBS rest sptr szLeft
  | szLeft <= 0  = return ()
  | otherwise    =
      do let (chunk1, chunk2) = BSS.splitAt szLeft f
             amt              = fromIntegral (BSS.length chunk1)
         BSS.useAsCString chunk1 $ \ dptr -> memcpy dptr sptr amt
         copyBS (chunk2 : rest) (sptr `plusPtr` amt) (szLeft - amt)

foreign import ccall unsafe "string.h memcpy"
  memcpy :: Ptr a -> Ptr b -> Int -> IO ()
