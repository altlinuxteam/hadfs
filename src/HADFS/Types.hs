module HADFS.Types where

--import HADFS.LDIF.Types
import Numeric (showHex)
import Data.Word

newtype Authority = Authority Int deriving (Eq, Show)
newtype RID = RID Int deriving (Eq, Show)

data ObjectSID =
  ObjectSID{ rev :: Int
           , subAuthCount :: Int
           , auth :: Authority
           , rids :: [RID]
           }
  deriving (Eq)
instance Show ObjectSID where
  show (ObjectSID r _ (Authority a) rs) = "S-" ++ show r ++ "-" ++ show a ++ concat (map (\(RID x) -> "-" ++ show x) rs)

data ObjectGUID =
  ObjectGUID{ f1 :: Word32
            , f2 :: Word16
            , f3 :: Word16
            , f4 :: Word64
            }
  deriving (Eq)
instance Show ObjectGUID where
  show (ObjectGUID f1 f2 f3 f4) = (showHex f1) "-" ++ showHex f2 "-" ++ showHex f3 "-" ++ f4_1 ++ "-" ++ f4_2
    where f4_h = showHex f4 ""
          f4_1 = take 4 f4_h
          f4_2 = drop 4 f4_h
