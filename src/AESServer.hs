-- Main.hs, final code
module AESServer (aesServer) where

import Control.Concurrent
import Control.Exception
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.ByteString.Char8 qualified as C
import Network.Socket
import Network.Socket.ByteString (recv, send, sendAll)
import System.IO

{------------------------------------------------

This server code is adapted from the haskell wiki article
on creating a chat server. The structure is largely unchanged
but it has been switched to use sockets instead of file
descriptors, and some other small tweaks

https://wiki.haskell.org/Implement_a_chat_server

-------------------------------------------------}

aesServer :: PortNumber -> IO ()
aesServer port = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 5520 (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 2
  chan <- newChan
  _ <- forkIO $
    fix $ \loop -> do
      (_, _) <- readChan chan
      loop
  mainLoop sock chan 0

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock
  forkIO (runConn conn chan msgNum)
  mainLoop sock chan $! msgNum + 1

--
-- a version of runConn that uses sockets instead of file handles
--
runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
  let broadcast msg = writeChan chan (msgNum, msg)

  -- putStrLn (show sock)
  putStrLn "sending greeting"
  sendAll sock $ C.pack "Hi, what's your name?"
  nameBS <- recv sock 1024
  let name = C.unpack nameBS
  putStrLn $ "new user joined: " ++ name
  broadcast ("--> " ++ name ++ " entered chat.")
  putStrLn "sending welcome"
  sendAll sock $ C.pack ("Welcome, " ++ name ++ "!")

  commLine <- dupChan chan

  -- fork off a thread for reading from the duplicated channel
  reader <- forkIO $
    fix $ \loop -> do
      (nextNum, line) <- readChan commLine
      putStr "intercepted: "
      putStrLn line
      when (msgNum /= nextNum) $ sendAll sock $ C.pack line
      loop

  handle (\(SomeException _) -> return ()) $
    fix $ \loop -> do
      lineBS <- recv sock 1024
      let line = C.unpack lineBS
      case line of
        -- If an exception is caught, send a message and break the loop
        "quit" -> sendAll sock $ C.pack "Bye!"
        -- else, continue looping.
        _ -> broadcast line >> loop

  killThread reader -- kill after the loop ends
  putStrLn $ name ++ " has left"
  broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
