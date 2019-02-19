{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module HADFS.AD where

import HADFS.AD.Types
import LDAP
import Data.Text (Text, intercalate, splitOn, pack, unpack, replace)
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
--import System.Log.FastLogger
import Data.Maybe
import Data.List (find, elem, notElem)
import qualified Data.Map as M
import qualified Data.Set as S
import HADFS.Types
import HADFS.Utils (entry2record, modopToLDAPMod, traceLog)
import HADFS.LDIF.Types

toObjCat :: String -> ObjectCategory
toObjCat "CN=Person" = Person
toObjCat _ = Default

getObjCat :: AD -> FilePath -> IO ObjectCategory
getObjCat ad path = do
  attrs <- nodeAttrs ad path ["objectCategory"]
  case getAttr "objectCategory" attrs of
    Nothing -> return Default
    Just x -> return $ toObjCat $ lastElem $ head x

addRealm :: AD -> DN -> DN
addRealm AD{..} (DN "") = realmDN
addRealm AD{..} dn =
  if T.isSuffixOf rdn dn' then dn else DN $ encodeUtf8 $ intercalate "," [dn', rdn]
  where rdn = fromDN realmDN
        dn' = fromDN dn

woRealm :: AD -> DN -> DN
woRealm AD{..} dn = DN $ encodeUtf8 $ fromJust $ T.stripSuffix rdn dn'
  where rdn = T.cons ',' $ fromDN realmDN
        dn' = fromDN dn

fromDN :: DN -> Text
fromDN (DN t) = decodeUtf8 t

fromPath :: FilePath -> DN
fromPath = DN . encodeUtf8 . intercalate "," . reverse . filter (/= "") . splitOn "/" . pack

realm2dn :: Text -> DN
realm2dn r = DN $ encodeUtf8 $ T.concat ["DC=", replace "." ",DC=" r]

initAD :: String -> String -> Int -> IO AD
initAD realm host port = do
  ldap <- ldapInit host (fromIntegral port)
  ldapGSSAPISaslBind ldap
  return $ AD ldap (realm2dn $ T.pack realm)

getAttr :: String -> [Entry] -> Maybe [String]
getAttr s ss =
  case find ((==s) . fst) ss of
    Just x -> Just $ snd x
    _ -> Nothing

nodeAttrs :: AD -> FilePath -> [String] -> IO [Entry]
nodeAttrs ad path attrs = do
  res <- ldapSearch (ldap ad) (Just dn) LdapScopeBase Nothing (LDAPAttrList attrs) True
  case res of
    [] -> return []
    _  -> return $ leattrs $ head res
  where dn = unpack . fromDN $ addRealm ad $ fromPath path

nodesAt :: AD -> FilePath -> IO [Node]
nodesAt ad path = do
--  res <- ldapSearch (ldap ad) (Just dn) LdapScopeOnelevel Nothing LDAPNoAttrs True
  res <- ldapSearch (ldap ad) (Just dn) LdapScopeOnelevel Nothing (LDAPAttrList ["objectCategory"]) True
  return $ map (\x -> (lastElem (ledn x), objCat x)) res
  where dn = unpack . fromDN $ addRealm ad $ fromPath path
--        lastElem = head . T.splitOn "," . T.pack . ledn
--        objCat x = toObjCat $ lastElem $ head $ fromJust $ getAttr "objectCategory" $ leattrs x
        objCat x = lastElem $ head $ fromJust $ getAttr "objectCategory" $ leattrs x

lastElem :: String -> String
lastElem = T.unpack . head . T.splitOn "," . T.pack

deleteNode :: AD -> FilePath -> IO ()
deleteNode ad path = ldapDelete (ldap ad) dn
  where dn = unpack . fromDN $ addRealm ad $ fromPath path

path2dn :: AD -> FilePath -> String
path2dn ad path = unpack . fromDN $ addRealm ad $ fromPath path

nodeRec :: AD -> FilePath -> [String] -> IO Record
nodeRec ad path attrs = do
  res <- ldapSearch (ldap ad) (Just dn) LdapScopeBase Nothing (LDAPAttrList attrs) True
  return $ entry2record $ head res
  where dn = unpack . fromDN $ addRealm ad $ fromPath path

children :: AD -> FilePath -> IO [Record]
children ad path = do
  res <- ldapSearch (ldap ad) (Just dn) LdapScopeOnelevel Nothing (LDAPAttrList ["objectCategory"]) True
  return $ map entry2record res
  where dn = unpack . fromDN $ addRealm ad $ fromPath path
        objCat x = lastElem $ head $ fromJust $ getAttr "objectCategory" $ leattrs x

search :: AD -> FilePath -> Text -> IO [Record]
search ad path req = do
  res <- ldapSearch (ldap ad) (Just dn) LdapScopeSubtree (Just filter) (LDAPAttrList attrs) False
  return $ map entry2record res
  where dn = unpack . fromDN $ addRealm ad $ fromPath path
        (filter:attrs) = (words . T.unpack) req

modify :: AD -> [ModOp] -> IO ()
modify ad mods =
  mapM_ (\(dn, ops) -> do
          traceLog $ "modify: " ++ dn ++ "\n" ++ show ops
          ldapModify (ldap ad) dn ops
      ) ldapmod
  where ldapmod = map modopToLDAPMod mods

moveNode :: AD -> FilePath -> FilePath -> IO ()
moveNode ad from to = do
  let [fromDN, toDN] = map (path2dn ad) [from, to]
      newParent =  drop 1 $ dropWhile (/=',') toDN
      toRDN = takeWhile (/=',') toDN
  traceLog $ "moverdn " ++ fromDN ++ " to " ++ toRDN ++ " parent " ++ newParent
--  ldapModrdn (ldap ad) fromDN toRDN
  ldapRename (ldap ad) fromDN toRDN newParent

setPass :: AD -> FilePath -> Text -> IO ()
setPass ad path pass = do
  traceLog $ "set pass: " ++ show pass
  ldapModify (ldap ad) dn [LDAPMod LdapModReplace "unicodePwd" [encodedPass]]
  where dn = unpack . fromDN $ addRealm ad $ fromPath path
        quotedPass = "\"" `T.append` pass `T.append` "\""
        encodedPass = (BS.unpack . encodeUtf16LE) quotedPass

add :: AD -> [ModOp] -> IO ()
add ad mods =
  mapM_ (\(dn, ops) -> do
          traceLog $ "create: " ++ dn ++ "\n" ++ show ops
          ldapAdd (ldap ad) dn ops
      ) ldapmod
  where ldapmod = map (modopToLDAPMod . filterFields) mods

filterFields :: ModOp -> ModOp
filterFields mop@(CreateRec _ (Attrs attrs)) = mop{ attrs = Attrs (M.filterWithKey (\k _ -> k `notElem` restrictedKeys) attrs)}
filterFields mop@(ModifyRec _ kops) = undefined

linkedFields :: [Key]
linkedFields = undefined

restrictedKeys :: [Key]
restrictedKeys =
  ["primaryGroupId"
  ,"cn"
  ,"distinguishedName"
  ,"memberOf"
  ,"name"
  ,"objectGUID"
  ,"objectSid"
  ,"primaryGroupID"
  ,"pwdLastSet" -- or MUST be 0 or -1
  ,"sAMAccountType"
  ,"uSNChanged"
  ,"uSNCreated"
  ,"whenChanged"
  ,"whenCreated"
  ]

{--
createMemberOf = CreateRec (DN "CN=bebebe1,CN=Users,DC=domain.DC=alt")  (mkAttrs [("memberOf", mkVals [Plain "CN=Domain Admins,CN=Users,DC=domain,DC=alt"])])

modifyMemberOf = ModifyRec (DN "CN=bebebe1,CN=Users,DC=domain.DC=alt")  [("memberOf", Add  $ mkVals [Plain "CN=Domain Admins,CN=Users,DC=domain,DC=alt"])]

convertLinkedFieldOp :: ModOp -> [ModOp]
convertLinkedFieldOp mop@(CreateRec dn (Attrs attrs)) =
  concat $
  map (\attr -> case attrToModOps attr of
                  Nothing -> [mop]
                  Just x -> x
      ) (M.toList attrs)

attrToModOps :: (Key, Vals) -> Maybe [ModOp]
attrToModOps ("memberOf", (Vals vals)) = Just $ map (\(Plain dn) -> ModifyRec (DN dn) [("member", Add (mkVals [Plain "DN Here"]))]) (S.toList vals)
attrToModOps _ = Nothing
--}
