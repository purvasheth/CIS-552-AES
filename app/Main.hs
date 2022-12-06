module Main where

import AESClient
import AESServer
import Data.Map qualified
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["server", p] -> AESServer.aesServer p []
    ["client", p] -> AESClient.aesClient p
    _ -> putStrLn "unknown command"
