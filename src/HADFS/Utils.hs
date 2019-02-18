module HADFS.Utils (
  guid2str,
  sid2str,
--  toLDIF,
--  fromLDIF,
  toRecords',
  entry2record,
  -- logging
  traceLog,
  modopToLDAPMod
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Base64 as B64
import Control.Monad
import Data.Text (Text, unpack)
import Data.Binary.Get
import Data.Either (fromRight)
import Data.Word
import Data.Bits
import HADFS.Types
import Data.ByteString.Base64
import Data.Char
import LDAP
import HADFS.LDIF.Types
import HADFS.LDIF.Utils
import qualified Data.Text as T
import HADFS.AD.Types (Entry)

type Entries = M.Map String [String]

getAuthority :: Get Authority
getAuthority = do
  d <- getByteString 6
  let auth = sum $ map (\(n, b) -> fromIntegral (b `shiftR` n)) $ zip [0,8..] (reverse (BS.unpack d))
  return $ Authority $ auth

getRID :: Get RID
getRID = do
  a <- getWord32le
  let rid = fromIntegral a
  return $ RID rid

parseSID' :: Get ObjectSID
parseSID' = do
  empty <- isEmpty
  if empty
    then return $ ObjectSID 0 0 (Authority 0) []
    else do r <- getWord8
            s <- getWord8
            a <- getAuthority
            rs <- replicateM (fromIntegral s) getRID
            return $ ObjectSID (fromIntegral r) (fromIntegral s) a rs

parseSID :: BL.ByteString -> ObjectSID
parseSID bs = runGet parseSID' bs

sid2str :: String -> String
sid2str s = show $ parseSID $ BL.pack s

parseGUID :: BL.ByteString -> ObjectGUID
parseGUID bs = runGet parseGUID' bs

parseGUID' :: Get ObjectGUID
parseGUID' = do
  empty <- isEmpty
  if empty
    then return $ ObjectGUID 0 0 0 0
    else do f1 <- getWord32le
            f2 <- getWord16le
            f3 <- getWord16le
            f4 <- getWord64be
            return $ ObjectGUID f1 f2 f3 f4

guid2str :: String -> String
guid2str s = show $ parseGUID $ BL.pack s


encodeIfNeeded :: String -> String
encodeIfNeeded s | (all isPrint) s = ": " ++ s
                 | otherwise = ":: " ++ (BC.unpack $ encode $ BC.pack s)

toStrings :: String -> [String] -> String
toStrings k vs | k == "objectGUID" = unlines $ [k ++ ": " ++ guid2str (head vs)]
               | k == "objectSid" = unlines $ [k ++ ": " ++ sid2str (head vs)]
               | otherwise = unlines $ fmap (\v -> k ++ encodeIfNeeded v) vs

e2le :: String -> [Entry] -> LDAPEntry
e2le dn e = LDAPEntry dn e

toRecords' :: String -> [Entry] -> [Record]
toRecords' dn e  = toRecords $ [e2le dn e]

toRecords :: [LDAPEntry] -> [Record]
toRecords = map entry2record

entry2record :: LDAPEntry -> Record
entry2record (LDAPEntry ledn leattrs) = Record ((DN (BC.pack ledn)), (l2a leattrs))
  where l2a :: [(String, [String])] -> Attrs
        l2a as = Attrs $ M.fromList $ map (\(k,v) -> ((Key (BC.pack k)), (mkVals (map attr2val v)))) as
        attr2val a = case (all isPrint) a of
          True -> Plain $ BC.pack a
          False -> Base64 $ encode $ BC.pack a

data Severity
  = ERROR
  | WARNING
  | INFO
  | DEBUG
  | TRACE
  deriving (Eq, Show, Enum)

writeLog :: Severity -> String -> IO ()
writeLog s m = putStrLn $ "[" ++ (show s) ++ "] " ++ m

traceLog :: String -> IO ()
traceLog m = writeLog TRACE m

modopToLDAPMod :: ModOp -> (String, [LDAPMod])
modopToLDAPMod (CreateRec (DN dn) (Attrs attrs)) =
  (BC.unpack dn
  ,M.foldrWithKey (
      \k (Vals vs) b -> b <> [LDAPMod LdapModAdd (BC.unpack (unKey k)) (map valToBinString (S.toList vs))]
      ) [] attrs)
modopToLDAPMod (ModifyRec (DN dn) aops) = (BC.unpack dn, concat (map (\(k, vs) -> aopToLDAPMod k vs) aops))

aopToLDAPMod :: Key -> AttrsOp -> [LDAPMod]
aopToLDAPMod k (Add     vals) = [vopToLDAPMod k (AddVals vals)]
aopToLDAPMod k (Delete  vals) = [vopToLDAPMod k (DeleteVals vals)]
aopToLDAPMod k (Modify  vops) = map (vopToLDAPMod k) vops
aopToLDAPMod k (Replace (Vals vals)) = [LDAPMod LdapModReplace (BC.unpack (unKey k)) $ map valToBinString $ S.toList vals]

vopToLDAPMod :: Key -> ValsOp -> LDAPMod
vopToLDAPMod k (DeleteVals vs) = LDAPMod LdapModDelete (BC.unpack (unKey k)) $ map valToBinString $ S.toList $ unVals vs
vopToLDAPMod k (AddVals vs) = LDAPMod LdapModAdd (BC.unpack (unKey k)) $ map valToBinString $ S.toList $ unVals vs

valToBinString :: Val -> String
valToBinString (Plain v) = BC.unpack v
valToBinString (Base64 v) = (BC.unpack . B64.decodeLenient) v
