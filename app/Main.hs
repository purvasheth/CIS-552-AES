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
    ["server"] -> AESServer.aesServer 5520
    ["client", key] -> AESClient.aesClient "5520" Nothing key
    _ -> putStrLn "unknown command"
