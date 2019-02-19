module HADFS.AD.Types where

import LDAP (LDAP)
import HADFS.LDIF.Types

data AD = AD {ldap :: LDAP, realmDN :: DN} deriving (Eq, Show)
type Entry = (String, [String])
type Node = (String, String)

data ADCtx = ADCtx{ ldapHost :: !String
                  , ldapPort :: !Int
                  }
  deriving (Eq, Show)


data ObjectCategory
  = Person
  | Default
  deriving (Eq, Show)

{--
import Data.Text (Text)
newtype DN = DN Text deriving (Eq, Show)




data Action
  = Create
  | Move
  | Delete
  | ChangeObjectCategory
  | Modify
  deriving (Eq, Show)

data UserAction
  = CreateUser
  | ChangePassword
  deriving (Eq, Show)
--}
