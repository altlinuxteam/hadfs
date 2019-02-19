module Main where

import qualified HADFS.FS as FS
import           System.Directory
import System.FilePath.Posix


main :: IO ()
main = do
  cwd <- getCurrentDirectory
  FS.init $ FS.Config "dc0.domain.alt" 389 "domain.alt" (cwd </> "mnt") cwd putStrLn
