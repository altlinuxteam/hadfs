module Main where

import qualified HADFS.FS as FS

main :: IO ()
main = FS.init "./mnt"
