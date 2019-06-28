{-# LANGUAGE OverloadedStrings, CPP #-}
module Main where

import Data.Maybe (fromMaybe)
import HADFS
import System.Directory
import System.Environment (getProgName, getArgs)
import System.FilePath
import Network.Socket
import Options.Applicative
#if MIN_VERSION_base(4,6,0)
import Control.Applicative
import Data.Semigroup ((<>))
#endif

mountpoint :: Parser FilePath
mountpoint = argument str (metavar "mountpoint" <> help "Mountpoint for AD LDAP tree")

hostname :: Parser String
hostname = argument str (metavar "host" <> help "Samba DC host name")

ldapport :: Parser Int
ldapport = argument auto (metavar "port" <> help "LDAP port" <> showDefault <> value 389)

data Opts = Opts {
    mp :: FilePath
  , h  :: String
  , p  :: Int
  } deriving (Show)

config :: Parser Opts
config = Opts <$> mountpoint <*> hostname <*> ldapport

opts :: ParserInfo Opts
opts = info (config <**> helper)
  (fullDesc <> progDesc "Mount AD LDAP settings tree as FUSE filesystem" <> header "Active Directory File System")

runner :: Opts -> IO ()
runner (Opts mp h p) = do
  progName <- getProgName
  cwd <- getCurrentDirectory
  domain <- tail . dropWhile (/='.') <$> getFQDN h

  let mountpoint = if head mp == '/' then mp else cwd </> mp
      logger = putStrLn
      cacheDir = cwd </> (".cache-" <> domain)

  hadfsInit domain h p mountpoint cacheDir logger

main :: IO ()
main = runner =<< execParser opts

getFQDN :: String -> IO String
getFQDN h = do
  addr:_ <- getAddrInfo (Just hints) (Just h) Nothing
  case addrCanonName addr of
    Nothing -> error $ "cannot get FQDN for " ++ h
    Just h' -> return h'
 where hints = defaultHints { addrFlags = [AI_CANONNAME] }
