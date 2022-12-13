module AESClient (startClient) where

import Control.Concurrent
import Control.Exception qualified as E
import Data.ByteString.Char8 qualified as C
import Decrypt
import Encrypt
import Network.Socket
import Network.Socket.ByteString (recv, send, sendAll)
import System.IO
import Utils

type Msg = (Int, String)

startClient :: String -> Key -> IO ()
startClient port key = do
  putStrLn "username: "
  name <- C.getLine
  setupSocket "127.0.0.1" port $ \s -> aesClient port s key name

-- Create a socket if we don't have one, then recursively call
-- the client on that socket
aesClient :: String -> Socket -> Key -> C.ByteString -> IO ()
aesClient port s key name = do
  chan <- newChan
  forkIO $ readFromSocket s key
  msgToSend <- C.getLine
  let prefixedMsg = name <> C.pack ": " <> msgToSend
  sendAll s $ C.concat $ map (getCipher key) (getBlocks prefixedMsg)
  aesClient port s key name

-- thread out our reads so we can still recieve while waiting for user input
readFromSocket :: Socket -> Key -> IO ()
readFromSocket sock k = do
  msg <- recv sock 1024
  C.putStrLn $ dcrpt msg k
  readFromSocket sock k
  where
    dcrpt msg k = case getString $ map (getPlainText k) (chunk 16 msg) of
      Nothing -> undefined
      (Just x) -> x

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