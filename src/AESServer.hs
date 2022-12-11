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

--aesServer :: IO ()
aesServer :: String -> [(Int, Socket)] -> IO ()
-- TODO remove socklist
aesServer port sockList = do
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
-- a version of runConn that uses sockets instead of file handles. nearly works!
--
runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
  let broadcast msg = writeChan chan (msgNum, msg)

  -- putStrLn (show sock)
  sendAll sock $ C.pack "Hi, what's your name?"
  nameBS <- recv sock 1024
  let name = C.unpack nameBS
  putStrLn $ "new user joined: " ++ name
  broadcast ("--> " ++ name ++ " entered chat.")
  sendAll sock $ C.pack ("Welcome, " ++ name ++ "!")

  commLine <- dupChan chan

  -- fork off a thread for reading from the duplicated channel
  reader <- forkIO $
    fix $ \loop -> do
      (nextNum, line) <- readChan commLine
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
        _ -> broadcast (name ++ ": " ++ line) >> loop

  killThread reader -- kill after the loop ends
  putStrLn $ name ++ " has left"
  broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
  -- hClose hdl -- close the handle

-- runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
-- runConn (sock, _) chan msgNum = do
--   let broadcast msg = writeChan chan (msgNum, msg)
--   hdl <- socketToHandle sock ReadWriteMode
--   hSetBuffering hdl NoBuffering
--
--   putStrLn (show sock)
--   hPutStrLn hdl "Hi, what's your name?"
--   name <- fmap init (hGetLine hdl)
--   -- let name = C.unpack nameBS
--   putStrLn $ "new user joined: " ++ name
--   broadcast ("--> " ++ name ++ " entered chat.")
--   hPutStrLn hdl ("Welcome, " ++ name ++ "!")
--
--   commLine <- dupChan chan
--
--   -- fork off a thread for reading from the duplicated channel
--   reader <- forkIO $
--     fix $ \loop -> do
--       (nextNum, line) <- readChan commLine
--       when (msgNum /= nextNum) $ hPutStrLn hdl line
--       loop
--
--   handle (\(SomeException _) -> return ()) $
--     fix $ \loop -> do
--       line <- fmap init (hGetLine hdl)
--       case line of
--         -- If an exception is caught, send a message and break the loop
--         "quit" -> hPutStrLn hdl "Bye!"
--         -- else, continue looping.
--         _ -> broadcast (name ++ ": " ++ line) >> loop
--
--   killThread reader -- kill after the loop ends
--   putStrLn $ name ++ " has left"
--   broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
--   hClose hdl -- close the handle

-- module AESServer (aesServer) where
--
-- import Control.Concurrent (forkFinally)
-- import Control.Exception qualified as E
-- import Control.Monad (forever, unless, void)
-- import Data.ByteString qualified as S
-- import Data.ByteString.Char8 qualified as C
-- import Data.Map (Map, insert)
-- import Network.Socket
-- import Network.Socket.ByteString (recv, send, sendAll)
--
-- aesServer :: String -> [(Int, Socket)] -> IO ()
-- aesServer port sockList = runTCPServer Nothing port (talk [])
--   where
--     talk :: [(Int, Socket)] -> Socket -> IO ()
--     talk sockList s = do
--       msg <- recv s 1024
--
--       let newSockList = (length sockList, s) : sockList
--       unless (S.null msg) $ do
--         putStr "intercepted: "
--         C.putStrLn msg
--         -- let encryptedMsg = encrypt (C.unpack msg)
--         sendAll s msg
--         -- sendToOthers msg (length sockList) newSockList
--         putStrLn (show newSockList)
--         talk newSockList s
--
-- sendToOthers :: S.ByteString -> Int -> [(Int, Socket)] -> IO ()
-- sendToOthers _ _ [] = return ()
-- sendToOthers msg i ((si, sock) : otherSocks) =
--   if i == si
--     then sendToOthers msg i otherSocks
--     else do
--       send sock msg
--       sendToOthers msg i otherSocks
--
-- -- from the "network-run" package.
-- runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
-- runTCPServer mhost port server = withSocketsDo $ do
--   addr <- resolve
--   E.bracket (open addr) close loop
--   where
--     resolve = do
--       let hints =
--             defaultHints
--               { addrFlags = [AI_PASSIVE],
--                 addrSocketType = Stream
--               }
--       head <$> getAddrInfo (Just hints) mhost (Just port)
--     open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
--       setSocketOption sock ReuseAddr 1
--       withFdSocket sock setCloseOnExecIfNeeded
--       bind sock $ addrAddress addr
--       listen sock 1024
--       return sock
--     loop sock = forever $
--       E.bracketOnError (accept sock) (close . fst) $
--         \(conn, _peer) ->
--           void $
--             -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
--             -- but 'E.bracketOnError' above will be necessary if some
--             -- non-atomic setups (e.g. spawning a subprocess to handle
--             -- @conn@) before proper cleanup of @conn@ is your case
--             forkFinally (server conn) (const $ gracefulClose conn 5000)