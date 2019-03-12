{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (fromMaybe)
import HADFS
import System.Directory
import System.Environment (getProgName, getArgs)
import System.FilePath

data Opts = Opts FilePath String Int deriving Show

parseOpts :: String -> [String] -> Opts
parseOpts _ [mp, h, p] = Opts mp h (read p)
parseOpts _ [mp, h] = Opts mp h 389
parseOpts prog _ = error $ "Usage: " ++ prog ++ " <mountpoint> <host> [port]"

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  cwd <- getCurrentDirectory
  let (Opts mp h p) = parseOpts progName args
      mountpoint = if head mp == '/' then mp else cwd </> mp
      logger = putStrLn
      domain = "domain.alt"
      cacheDir = cwd </> (".cache-" <> domain)

  hadfsInit domain h p mountpoint cacheDir logger
