module AESClient (aesClient) where

import Control.Concurrent
import Control.Exception qualified as E
import Data.ByteString.Char8 qualified as C
import Decrypt
import Encrypt
import Network.Socket
import Network.Socket.ByteString (recv, send, sendAll)
import System.IO

type Msg = (Int, String)

-- Create a socket if we don't have one, then recursively call
-- the client on that socket
aesClient :: String -> Maybe Socket -> String -> IO ()
aesClient port ms key = case ms of
  Nothing -> setupSocket "127.0.0.1" port $ \s -> aesClient port (Just s) key
  Just s -> do
    chan <- newChan
    forkIO $ readFromSocket s
    msgToSend <- getLine
    sendAll s (C.pack msgToSend)
    aesClient port (Just s) key

-- thread out our reads so we can still recieve while waiting for user input
readFromSocket :: Socket -> IO ()
readFromSocket sock = do
  msg <- recv sock 1024
  C.putStrLn msg
  readFromSocket sock

{-
This function was taken from the simple echo client example here:
https://hackage.haskell.org/package/network-3.1.2.7/docs/Network-Socket.html
-}
setupSocket :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
setupSocket host port client = withSocketsDo $ do
  addr <- resolve
  commLine <- newChan
  E.bracket (open addr) close client
  where
    resolve = do
      let hints = defaultHints {addrSocketType = Stream}
      head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock