{-# LANGUAGE OverloadedStrings #-}
module Main where

import HADFS
import System.Directory
import System.FilePath

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let mountpoint = cwd </> "mnt"
      logger = putStrLn
      realm = "domain.alt"
      host = "dc0.domain.alt"
      port = 389
      cacheDir = cwd </> (".cache-" <> realm)

  hadfsInit realm host port mountpoint cacheDir logger
