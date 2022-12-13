module AESClient (aesClient) where

-- import Control.Concurrent
-- import Control.Exception (SomeException (SomeException), handle)
-- import Control.Exception qualified as E
-- import Control.Monad.Fix (fix)
-- import Data.ByteString.Char8 qualified as C
-- import Network.Socket
-- import Network.Socket.ByteString (recv, send, sendAll, sendTo)
-- import System.IO
import Control.Concurrent
import Control.Exception
import Control.Exception qualified as E
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.ByteString.Char8 qualified as C
import Network.Socket
import Network.Socket.ByteString (recv, send, sendAll)
import System.IO

type Msg = (Int, String)

-- TODO remove socklist
-- aesClient :: String -> Maybe Socket -> IO ()
-- aesClient port sockList = do
--   sock <- socket AF_INET Stream 0
--   setSocketOption sock ReuseAddr 1
--   connect sock (SockAddrInet 5520 (tupleToHostAddress (127, 0, 0, 1)))
--   addr <- resolve
--   -- E.bracket (open addr) close client
--   -- listen sock 2
--   chan <- newChan
--   _ <- forkIO $
--     fix $ \loop -> do
--       (_, _) <- readChan chan
--       loop
--   mainLoop sock chan 0
--   where
--     resolve = do
--       let hints = defaultHints {addrSocketType = Stream}
--       head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just port)
--     open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
--       connect sock $ addrAddress addr
--       return sock
--
-- mainLoop :: Socket -> Chan Msg -> Int -> IO ()
-- mainLoop sock chan msgNum = do
--   -- conn <- accept sock
--   forkIO (runConn sock chan msgNum)
--   mainLoop sock chan $! msgNum + 1
--
-- runConn :: Socket -> Chan Msg -> Int -> IO ()
-- runConn sock chan msgNum = do
--   commLine <- dupChan chan
--
--   -- fork off a thread for reading from the duplicated channel
--   reader <- forkIO $
--     fix $ \loop -> do
--       (nextNum, line) <- readChan commLine
--       when (msgNum /= nextNum) $ sendAll sock $ C.pack line
--       loop
--
--   handle (\(SomeException _) -> return ()) $
--     fix $ \loop -> do
--       -- lineBS <- recv sock 1024
--       -- let line = C.unpack lineBS
--       line <- getLine
--       case line of
--         -- If an exception is caught, send a message and break the loop
--         "quit" -> sendAll sock $ C.pack "Bye!"
--         -- else, continue looping.
--         _ -> sendAll sock (C.pack line) >> loop
--
--   killThread reader -- kill after the loop ends
--   -- putStrLn $ name ++ " has left"
--   -- broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast

-- aesClient :: String -> IO ()
-- aesClient port = runTCPClient "127.0.0.1" port $ \s -> do
--   h <- socketToHandle s ReadWriteMode
--   aesClient2 h
--
--   return ()
--
-- aesClient2 :: Handle -> IO ()
-- aesClient2 h = do
--   -- h <- hio
--   putStrLn (show h)
--   msgToSend <- getLine
--   hPutStrLn h msgToSend
--   msg <- fmap init (hGetLine h)
--   putStr "Received: "
--   hPutStrLn h msg
--   aesClient2 h

aesClient :: String -> Maybe Socket -> Int -> IO ()
aesClient port ms msgNum = case ms of
  Nothing -> runTCPClient "127.0.0.1" port $ \s -> aesClient port (Just s) msgNum
  Just s -> do
    chan <- newChan
    forkIO (runConn s chan msgNum)
    msgToSend <- getLine
    sendAll s (C.pack msgToSend)
    msg <- recv s 1024
    -- putStr "Received: "
    C.putStrLn msg
    aesClient port (Just s) (msgNum + 1)

--
-- -- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
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

runConn :: Socket -> Chan Msg -> Int -> IO ()
runConn sock chan msgNum = do
  commLine <- dupChan chan

  -- fork off a thread for reading from the duplicated channel
  reader <- forkIO $
    fix $ \loop -> do
      (nextNum, line) <- readChan commLine
      when (msgNum /= nextNum) $ sendAll sock $ C.pack line
      loop

  killThread reader -- kill after the loop ends
  -- putStrLn $ name ++ " has left"
  -- broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast