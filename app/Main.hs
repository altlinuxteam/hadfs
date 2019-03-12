{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (fromMaybe)
import HADFS
import System.Directory
import System.Environment (getProgName, getArgs)
import System.FilePath
import Network.Socket

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
  domain <- tail . dropWhile (/='.') <$> getFQDN h

  let mountpoint = if head mp == '/' then mp else cwd </> mp
      logger = putStrLn
      cacheDir = cwd </> (".cache-" <> domain)

  hadfsInit domain h p mountpoint cacheDir logger

getFQDN :: String -> IO String
getFQDN h = do
  addr:_ <- getAddrInfo (Just hints) (Just h) Nothing
  case addrCanonName addr of
    Nothing -> error $ "cannot get FQDN for " ++ h
    Just h' -> return h'
 where hints = defaultHints { addrFlags = [AI_CANONNAME] }
