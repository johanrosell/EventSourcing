{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Person where

import           Aggregate    (Aggregate, Command (exec), Zero, increment, zero)
import           Data.Aeson   (ToJSON)
import           GHC.Generics

data Person = Person
    { name :: String
    , age  :: Int }
    deriving (Generic, Show)

data Event =
      Created Person
    | Aged Int
    | Deleted
    deriving (Generic, Show)

data Cmd =
      Create Person
    | Delete
    deriving (Show)

instance Zero Person where
    zero = Person { name = "", age = 0 }

instance Aggregate Person Event where
    increment s (Aged x)    = Person (name s) (age s + x)
    increment _ (Created p) = p
    increment _ Deleted     = zero

instance Command Person Event Cmd where
    exec _ c =
        case c of
            Create p -> Right [Created p]
            Delete   -> Right [Deleted]

instance ToJSON Event
instance ToJSON Person
