{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Person where

import           Aggregate                  (Aggregate, Zero, update, zero)
import           Control.Monad.Trans.Except (ExceptT)
import           Data.Aeson                 (FromJSON, ToJSON)
import           GHC.Generics

data Person = Person
    { name :: String
    , age  :: Int }
    deriving (Generic, Show)

data Event =
      Created Person
    | Aged Int
    deriving (Generic, Show)

instance Zero Person where
    zero = Person { name = "", age = 0 }

instance Aggregate Person Event where
    update s (Aged x)    = Person (name s) (age s + x)
    update _ (Created p) = p

instance ToJSON Event
instance ToJSON Person
