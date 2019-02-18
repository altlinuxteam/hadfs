{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module HADFS.FS where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import Control.Monad
import Foreign.C.Error
import System.PosixCompat.Types
import System.PosixCompat.Files
import System.IO
import System.FilePath.Posix
import System.Directory
import Control.Concurrent
import Data.Maybe
import Data.Either
import System.Fuse
import Data.Text (unpack)
import Data.List (isPrefixOf)
import HADFS.AD
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import HADFS.AD.Types
import qualified HADFS.AD
import HADFS.Types
import HADFS.Utils
import HADFS.LDIF.Utils
import HADFS.LDIF.Types
import Control.Exception.Base

type HT = ()

type FSMap = M.Map FilePath FileStat

data State =
  State{ ad :: AD
       , tmp :: FilePath
       }
  deriving (Show)

init :: FilePath -> IO ()
init mp = do
  let prog = "hadfs"
  putStrLn "Initialize AD..."
  ad <- HADFS.AD.init "domain.alt"
  cwd <- getCurrentDirectory
  let tmp = cwd </> ".cache"
  createDirectoryIfMissing False tmp
  writeFile (tmp </> ".dummy") ""
  fuseRun prog [mp,"-o", "default_permissions,auto_unmount", "-f", "-o", "subtype=dc0.domain.alt"] (adFSOps (State ad tmp)) defaultExceptionHandler

adFSOps :: State -> FuseOperations HT
adFSOps s = defaultFuseOps { fuseGetFileStat = getFileStat s
                           , fuseOpen        = openF s
                           , fuseRead        = readF s
                           , fuseOpenDirectory = openD s
                           , fuseReadDirectory = readD s
                           , fuseGetFileSystemStats = getFileSystemStats s
                           -- create operations
                           , fuseCreateDirectory = mkdir s
                           , fuseCreateDevice = create s
                           -- delete operations
                           , fuseRemoveLink = unlink s
                           , fuseRemoveDirectory = rmdir s
                           -- write operations
                           , fuseSetFileSize = setFSize s
                           , fuseWrite = write s
                           -- other
                           , fuseSetFileTimes = setFTime s
                           , fuseRename = mv s
                           }

getFileStat :: State -> FilePath -> IO (Either Errno FileStat)
getFileStat State{..} path | takeFileName path == ".refresh" = do
                               status <- getFileStatus (tmp </> ".dummy")
                               return $ Right $ fromStatus status
getFileStat State{..} path =
  fileExist cachePath >>= \x -> if x then do status <- getFileStatus cachePath
                                             return $ Right $ fromStatus status else return $ Left eNOENT
  where cachePath = tmp </> tail path

openD State{..} "/" = return eOK
openD State{..} path = do
  createDirectoryIfMissing True (tmp </> tail path)
  return eOK

refreshHooks :: State -> ObjectCategory -> FilePath -> IO ()
refreshHooks st@State{..} Person path = do
  refreshHooks st Default path
  exists <- doesFileExist (cachePath </> ".chpwd")
  when exists $ removeFile (cachePath </> ".chpwd")
  let hooks = [(cachePath </> ".chpwd", "")]
  mapM_ (uncurry createIfNotDeleted) hooks
  where cachePath = tmp </> tail path

refreshHooks st@State{..} Default path = do
  ctx <- getFuseContext
--  attrs <- nodeAttrs ad path ["*", "+", "nTSecurityDescriptor"]
  record <- nodeRec ad path ["*", "+"]
  let hooks = [(cachePath </> ".attributes", T.unpack (recordToLDIF record))
              ]
  mapM_ (uncurry createIfNotDeleted) hooks
  where cachePath = tmp </> tail path

createIfNotDeleted :: FilePath -> String -> IO ()
createIfNotDeleted path content = do
  exists <- fileExist ph
  if exists
    then return ()
    else writeFile path content
  where ph = takeDirectory path </> "." ++ fileName
        fileName = takeFileName path

refresh :: State -> FilePath -> IO ()
refresh st@State{..} path = do
  nodes <- nodesAt ad path
  attrs <- nodeAttrs ad path ["objectCategory"]
  let objCat = toObjCat cat
      cat = lastElem $ head $ fromJust $ getAttr "objectCategory" attrs
  mapM_ (\(x, cat) -> createDirectoryIfMissing True (tmp </> tail path </> x)) nodes
  refreshHooks st objCat path

fromStatus :: FileStatus -> FileStat
fromStatus st =
  FileStat { statEntryType = entryType st
           , statFileMode = fileMode st
           , statLinkCount = linkCount st
           , statFileOwner = fileOwner st
           , statFileGroup = fileGroup st
           , statSpecialDeviceID = specialDeviceID st
           , statFileSize = fileSize st
           , statBlocks = 1
           , statAccessTime = accessTime st
           , statModificationTime = modificationTime st
           , statStatusChangeTime = statusChangeTime st
           }

entryType :: FileStatus -> EntryType
entryType m | isDirectory m = Directory
            | otherwise = RegularFile

readD :: State -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
readD st@State{..} path = do
  ctx <- getFuseContext
  refresh st path
  content <- listDirectory (tmp </> tail path) >>= \x -> return $ filter (not . isPrefixOf "..") x
  stats <- mapM (\x -> fromStatus <$> getFileStatus (tmp </> tail path </> x)) content
  return $ Right $ zip content stats

--readD _ _ = return (Left (eNOENT))

openF :: State -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
openF State{..} path mode flags = return $ Right ()

readF :: State -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
readF st@State{..} path _ byteCount offset = do
  putStrLn $ "read file:" ++ path
  withFile (tmp </> tail path) ReadMode
    (\fh -> do
        content <- B.hGetContents fh
        return $ Right content
    )

getFileSystemStats :: State -> String -> IO (Either Errno FileSystemStats)
getFileSystemStats State{..} str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }

unlink :: State -> FilePath -> IO Errno
unlink st@State{..} path = do
  traceLog $ "unlink: " ++ show path
  renameFile cachePath newName
  return eOK
  where cachePath = tmp </> tail path
        newName = cacheDir </> "." ++ fileName
        cacheDir = takeDirectory cachePath
        fileName = takeFileName cachePath

rmdir :: State -> FilePath -> IO Errno
rmdir st@State{..} path = do
  deleteNode ad path
  removeDirectoryRecursive cachePath
  return eOK
  where cachePath = tmp </> tail path

cleanCacheDir :: State -> FilePath -> IO Errno
cleanCacheDir State{..} path = do
  let cacheDir = tmp </> tail (takeDirectory path)
  files <- listDirectory cacheDir
  mapM_ (\n -> removeFile (cacheDir </> n)) files
  return eOK

-- create operations
create :: State -> FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
create st@State{..} path RegularFile mode _ | takeFileName path == ".refresh" = cleanCacheDir st path
                                            | takeFileName path == ".attributes" = do
                                                let cacheDir = tmp </> tail (takeDirectory path)
                                                traceLog "create .attributes"
                                                files <- listDirectory cacheDir
                                                case files of
                                                  [] -> do
                                                    writeFile (tmp </> tail path) "dn: new record\n"
                                                    return eOK
                                                  _ -> return eACCES
                                            | ".search" `isPrefixOf` takeFileName path = do
                                                let cacheDir = tmp </> tail (takeDirectory path)
                                                traceLog "create .search handler"
                                                writeFile (cacheDir </> takeFileName path) ""
                                                return eOK
                                            | ".chpwd" == takeFileName path = do
                                                let cacheDir = tmp </> tail (takeDirectory path)
                                                traceLog $ show path ++ " created"
                                                writeFile (cacheDir </> takeFileName path) ""
                                                return eOK
                                            | otherwise = return eACCES

setFTime :: State -> FilePath -> EpochTime -> EpochTime -> IO Errno
setFTime st path _ _ | takeFileName path == ".refresh" = cleanCacheDir st path
                     | otherwise = return eOK

-- write operations
setFSize :: State -> FilePath -> FileOffset -> IO Errno
setFSize st@State{..} path offset | takeFileName path == ".refresh" = cleanCacheDir st path
                                  | otherwise = do
                                      traceLog $ "setFSize: " ++ path ++ " (" ++ show offset ++ ")"
                                      return eOK

write :: State -> FilePath -> HT -> ByteString -> FileOffset -> IO (Either Errno ByteCount)
write st@State{..} path _ content offset | takeFileName path == ".attributes" = writeAttrs st path () content offset
                                         | takeFileName path == ".refresh" = return $ Right $ fromIntegral $ BS.length content
                                         | takeFileName path == ".chpwd" = changeUserPassword st path () content offset
                                         | ".search" `isPrefixOf` takeFileName path = searchHandler st path () content offset
                                         | otherwise = return $ Left eNOSYS

changeUserPassword :: State -> FilePath -> HT -> ByteString -> FileOffset -> IO (Either Errno ByteCount)
changeUserPassword State{..} path _ content offset | content == BS.empty = do
                                                       traceLog "empty password"
                                                       return $ Right $ fromIntegral $ BS.length content
                                                   | otherwise = do
                                                       traceLog $ "change password for " ++ show (takeDirectory path)
                                                       setPass ad (takeDirectory path) (T.decodeUtf8 content)
                                                       return $ Right $ fromIntegral $ BS.length content

writeAttrs :: State -> FilePath -> HT -> ByteString -> FileOffset -> IO (Either Errno ByteCount)
writeAttrs st@State{..} path _ content offset = do
  traceLog $ "write attributes to:" ++ path ++ " with offset: " ++ (show offset) ++ " content: " ++ (show content)
  oldCont <- BS.readFile cachePath
--  traceLog $ show $ fromLDIF (T.decodeUtf8 oldCont)
--  traceLog $ show $ fromLDIF (T.decodeUtf8 content)
  let diff = T.encodeUtf8 $ T.unlines $ modopToLDIF $ cmp oldRec newRec
      oldRec = head $ fromRight [] $ fromLDIF (T.decodeUtf8 oldCont)
      contentLength = fromIntegral $ BS.length content
      newRec = case offset of
                 0 -> head $ fromRight [] $ fromLDIF (T.decodeUtf8 content)
                 x -> head $ fromRight [] $ fromLDIF (T.decodeUtf8 (
                                                     (BS.take (fromIntegral offset) oldCont) `BS.append` content `BS.append` (BS.drop ((fromIntegral offset) + contentLength) oldCont))
                                                     )
      modOp = cmp oldRec newRec
--  traceLog $ "oldRec: " ++ (show oldRec)
--  traceLog $ "newRec: " ++ (show newRec)
  traceLog $ "cmp: " ++ show (cmp oldRec newRec)
--  traceLog $ "diff: " ++ (show diff)
  case modOp of
    CreateRec _ _ -> add ad [modOp]
    _ -> modify ad [modOp]

--  BS.writeFile (cachePath <.> ".change") diff
  return $ Right $ fromIntegral $ BS.length content
  where cachePath = tmp </> tail path

mkdir :: State -> FilePath -> FileMode -> IO Errno
mkdir st@State{..} path mode = do
  traceLog $ "mkdir: " ++ path
  createDirectoryIfMissing True (tmp </> tail path)
  return eOK

searchHandler :: State -> FilePath -> HT -> ByteString -> FileOffset -> IO (Either Errno ByteCount)
searchHandler st@State{..} path _ content offset = do
  traceLog $ "handle search for: " ++ path
  res <- search ad (takeDirectory path) (T.decodeUtf8 content)
  let searchRes = T.encodeUtf8 $ T.unlines $ map recordToLDIF res
  traceLog $ "write content of: " ++ cachePath
  BS.writeFile cachePath content
  traceLog $ "write results to: " ++ cacheDir
--  traceLog $ "results: " ++ (show searchRes)
  BS.writeFile (cachePath <.> ".result") searchRes
  return $ Right $ fromIntegral $ BS.length content
  where cacheDir = tmp </> tail (takeDirectory path)
        cachePath = cacheDir </> takeFileName path

mv :: State -> FilePath -> FilePath -> IO Errno
mv State{..} from to = do
  traceLog $ "move " ++ from ++ " to " ++ to
  moveNode ad from to
  removeDirectoryRecursive cacheDir
  return eOK
  where cacheDir = tmp </> tail from
