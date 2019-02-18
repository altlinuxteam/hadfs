{-# LANGUAGE
OverloadedStrings,
GeneralizedNewtypeDeriving
#-}
module HADFS.LDIF.Types where
import Data.String (IsString(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.ByteString (ByteString)

data Val = Plain ByteString
         | Base64 ByteString
  deriving (Eq, Ord, Show)
newtype DN = DN {unDN :: ByteString} deriving (Eq, Show, IsString)
newtype Key = Key {unKey :: ByteString} deriving (Eq, Ord, Show, IsString)
newtype Vals = Vals {unVals :: Set Val} deriving (Eq, Show)
newtype Attrs = Attrs {unAttr :: Map Key Vals} deriving (Eq, Show)
newtype Record = Record {unRecord :: (DN, Attrs)} deriving (Eq, Show)

data ValsOp = AddVals Vals
            | DeleteVals Vals
  deriving (Eq, Show)

instance Semigroup Vals where
  (Vals a) <> (Vals b) = Vals $ a <> b

instance Semigroup Attrs where
  (Attrs a) <> (Attrs b) = Attrs $ M.unionWith (<>) a b

instance Monoid Attrs where
  mempty = Attrs M.empty

unValsOp :: ValsOp -> Vals
unValsOp (AddVals vs) = vs
unValsOp (DeleteVals vs) = vs

data AttrsOp = Add Vals        -- add new attribute
             | Delete Vals     -- delete whole attribute
             | Modify [ValsOp] -- add or delete values of attribute
             | Replace Vals    -- replace values with new one
  deriving (Eq, Show)

data ModOp = CreateRec{ dn :: DN, attrs :: Attrs}
           | ModifyRec{ dn :: DN, ops :: [(Key, AttrsOp)]}
  deriving (Eq, Show)

mkVals :: [Val] -> Vals
mkVals = Vals <$> S.fromList

mkAttrs :: [(Key, Vals)] -> Attrs
mkAttrs = Attrs <$> M.fromList

{--
mkModOp :: DN -> [(Key, AttrsOp)] -> ModOp
mkModOp dn aops = ModOp (dn, aops)
--}
