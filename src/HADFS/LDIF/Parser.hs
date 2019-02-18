{-# LANGUAGE OverloadedStrings #-}
module HADFS.LDIF.Parser where

import Prelude hiding (takeWhile)
import Data.Text (Text, cons, append, pack)
import Data.Text.Encoding
import Data.Attoparsec.Text
import qualified Data.Map as M
import qualified Data.Set as S
import HADFS.LDIF.Types -- (Record(..), Attrs(..), Val(..))

newtype AttrIR = AttrIR (Text, Val) deriving (Eq, Show)
newtype RecordIR = RecordIR (Text, [AttrIR]) deriving (Eq, Show)

parse :: Text -> Either String [Record]
parse ldif = case parseOnly ldifP' ldif of
  Left e -> Left $ "parse error with: " ++ e
  Right rs -> Right $ convert rs


toVal :: [AttrIR] -> Attrs
toVal = foldr (\(AttrIR (k, v)) m -> m <> Attrs (M.singleton (Key (encodeUtf8 k)) (mkVals [v]))) (Attrs M.empty)

convert :: [RecordIR] -> [Record]
convert = map (\(RecordIR (dn, attrs)) -> Record (DN (encodeUtf8 dn), toVal attrs))

ldifP' :: Parser [RecordIR]
ldifP' = do
  skipTrash
  many1 recordP

recordP :: Parser RecordIR
recordP = do
  many' comment
  dn <- dnP
  many' comment
  attrs <- many' attrP
  many' comment
  choice [eol, endOfInput]
  return $ RecordIR (dn,attrs)

dnP :: Parser Text
dnP = do
  string "dn:"
  skipSpace
  dn <- takeTill isEol
  many' comment
  eol
  return dn

attrP :: Parser AttrIR
attrP = do
  many' comment
  key <- keyP
  many' comment
  val <- valP
  many' comment
  choice [eol, endOfInput]
  return $ AttrIR (key, val)

keyP :: Parser Text
keyP = do
  first <- letter
  rest <- takeTill (==':')
  return $ cons first rest

valP :: Parser Val
valP = choice [textVal, base64Val]

textVal :: Parser Val
textVal = do
  string ": "
  Plain . encodeUtf8 <$> val'

base64Val :: Parser Val
base64Val = do
  string ":: "
  Base64 . encodeUtf8 <$> val'

val' :: Parser Text
val' = do
  v <- takeTill isEol
  rs <- option "" rest
  return (v `append` rs)
  where rest :: Parser Text
        rest = do
          string "\n "
          skipWhile (==' ')
          takeTill isEol

comment :: Parser ()
comment = do
  char '#'
  skipWhile (not . isEol)
  eol

skipTrash :: Parser ()
skipTrash = do
  many' $ choice [eol, comment]
  return ()

isEol :: Char -> Bool
isEol = isEndOfLine

eol :: Parser ()
eol = endOfLine
