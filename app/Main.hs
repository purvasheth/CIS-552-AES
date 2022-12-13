module Main where

import AESClient
import AESServer
import Data.Map qualified
import System.Environment
import Utils

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["server", p] -> AESServer.aesServer p []
    ["client", p] -> AESClient.aesClient p Nothing 0
    _ -> putStrLn "unknown command"
