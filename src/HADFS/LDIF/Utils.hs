{-# LANGUAGE OverloadedStrings #-}
module HADFS.LDIF.Utils where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.List as L
import HADFS.LDIF.Types
import HADFS.LDIF.Parser (parse)

fromLDIF :: Text -> Either String [Record]
fromLDIF = parse

cmp :: Record -> Record -> ModOp
cmp (Record ((DN oldDN), oldAttrs)) (Record ((DN newDN), newAttrs)) | oldDN == "new record" = CreateRec (DN newDN) newAttrs
                                                                    | otherwise = ModifyRec (DN oldDN) (cmpAttrs oldAttrs newAttrs)

cmpVals :: Vals -> Vals -> (ValsOp, ValsOp)
cmpVals (Vals old) (Vals new) =
  let deleted = DeleteVals $ Vals $ S.difference old new
      added   = AddVals    $ Vals $ S.difference new old
  in (added, deleted)

cmpAttrs :: Attrs -> Attrs -> [(Key, AttrsOp)]
cmpAttrs (Attrs old) (Attrs new) =
  let mm = M.intersection old new
      addAttrs = M.map Add    $ M.difference new old
      delAttrs = M.map Delete $ M.difference old new
      modAttrs = M.intersectionWith (
        \a b -> let (add, del) = cmpVals a b
                    opsCount = S.size $ (unVals.unValsOp) add <> (unVals.unValsOp) del
                    newRecSize = S.size (unVals b)
                in if opsCount == 0 then Nothing
                   else Just $ case opsCount `compare` newRecSize of
                                 LT -> Modify $ normValOps [add, del]
                                 _ -> Replace b
        ) old new
  in M.toList $ addAttrs <> delAttrs <> (M.mapMaybe id modAttrs)

normValOps :: [ValsOp] -> [ValsOp]
normValOps = filter (not . isEmpty)
  where isEmpty :: ValsOp -> Bool
        isEmpty (AddVals    (Vals vs)) = (vs == S.empty)
        isEmpty (DeleteVals (Vals vs)) = (vs == S.empty)

modopsToLDIF :: [ModOp] -> Text
modopsToLDIF = T.unlines . concat . (intersperse [""]) . (map modopToLDIF)

modopToLDIF :: ModOp -> [Text]
modopToLDIF (CreateRec (DN dn) (Attrs attrs)) = [T.pack $ show attrs] --["dn: " <> T.decodeUtf8 dn] <> (M.map (\(k, vs) -> (k, map valToLDIF vs)) attrs)
modopToLDIF (ModifyRec (DN dn) aops)  = ["dn: " <> T.decodeUtf8 dn] <> aopsToLDIF aops

aopsToLDIF :: [(Key, AttrsOp)] -> [Text]
aopsToLDIF ops = ["changetype: modify"] <> (concat $ intersperse ["-"] $ map (uncurry aopToLDIF) ops)

aopToLDIF :: Key -> AttrsOp -> [Text]
aopToLDIF key         (Add     vals) = vopsToLDIF key (AddVals vals)
aopToLDIF (Key k)     (Delete  _   ) = ["delete: "  <> T.decodeUtf8 k]
aopToLDIF key         (Modify  vops) = concat (map (vopsToLDIF key) vops)
aopToLDIF key@(Key k) (Replace vals) = ["replace: " <> T.decodeUtf8 k] <> attrToLDIF key vals

attrToLDIF :: Key -> Vals -> [Text]
attrToLDIF k (Vals vs) = map (valToLDIF k) $ S.toList vs

attrsToLDIF :: Attrs -> [Text]
attrsToLDIF (Attrs attrs) = concat $ map (\(k,vs) -> attrToLDIF k vs) $ M.toList attrs

valToLDIF :: Key -> Val -> Text
valToLDIF k (Plain  v) = T.decodeUtf8 $ BS.concat [unKey k, ": ",  v]
valToLDIF k (Base64 v) = T.decodeUtf8 $ BS.concat [unKey k, ":: ", v]

vopsToLDIF :: Key -> ValsOp -> [Text]
vopsToLDIF key@(Key k) (AddVals    vs) = case S.size (unVals vs) of
  0 -> []
  _ -> ["add: "    <> T.decodeUtf8 k] <> attrToLDIF key vs
vopsToLDIF key@(Key k) (DeleteVals vs) = case S.size (unVals vs) of
  0 -> []
  _ -> ["delete: " <> T.decodeUtf8 k] <> attrToLDIF key vs

recordToLDIF :: Record -> Text
recordToLDIF (Record ((DN dn), attrs)) = T.unlines $ ["dn: " <> T.decodeUtf8 dn] <> attrsToLDIF attrs
