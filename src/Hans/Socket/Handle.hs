module Hans.Socket.Handle(makeHansHandle) where

import Control.Exception
import Data.Typeable
import GHC.IO.BufferedIO
import GHC.IO.Device
import GHC.IO.Handle
import Hans.NetworkStack
import System.IO

data BufferedSocket = BS {
    bsSocket :: Socket
  }
 deriving (Typeable)

newBufferedSocket :: Socket -> IO BufferedSocket
newBufferedSocket sock =
  do return (BS sock)

instance IODevice BufferedSocket where
  ready _ _ _  = undefined
  close        = undefined
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
  dup _        = undefined
  dup2 _ _     = undefined

instance BufferedIO BufferedSocket where
  newBuffer    = undefined
  fillReadBuffer = undefined
  fillReadBuffer0 = undefined
  emptyWriteBuffer = undefined
  flushWriteBuffer = undefined
  flushWriteBuffer0 = undefined 

makeHansHandle :: Socket -> IOMode -> IO Handle
makeHansHandle socket mode =
  do buffSocket <- newBufferedSocket socket
     mkFileHandle buffSocket "<socket>" mode Nothing noNewlineTranslation
