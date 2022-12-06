module AESServer (aesServer) where

import Control.Concurrent (forkFinally)
import Control.Exception qualified as E
import Control.Monad (forever, unless, void)
import Data.ByteString qualified as S
import Data.ByteString.Char8 qualified as C
import Data.Map (Map, insert)
import Network.Socket
import Network.Socket.ByteString (recv, send, sendAll)

aesServer :: String -> [(Int, Socket)] -> IO ()
aesServer port sockList = runTCPServer Nothing port talk
  where
    talk s = do
      msg <- recv s 1024

      let newSockList = (length sockList, s) : sockList
      unless (S.null msg) $ do
        putStr "intercepted: "
        C.putStrLn msg
        sendAll s msg
        --sendToOthers msg (length sockList) newSockList
        talk s

sendToOthers :: S.ByteString -> Int -> [(Int, Socket)] -> IO ()
sendToOthers _ _ [] = return ()
sendToOthers msg i ((si, sock) : otherSocks) =
  if i == si
    then sendToOthers msg i otherSocks
    else do
      send sock msg
      sendToOthers msg i otherSocks

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = forever $
      E.bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) ->
          void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 5000)