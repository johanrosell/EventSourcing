{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import           Aggregate    (Aggregate, Command (exec), Zero, applyEvent,
                               zero)
import           Data.Aeson   (ToJSON)
import           GHC.Generics

newtype Event = ValueRegistered Int
  deriving (Generic, Show, Eq)

newtype State = State { eventLog :: [Event]}
    deriving (Generic, Show)

newtype Cmd = RegisterValue Int

instance Zero State where
    zero = State []

instance Aggregate Event State where
    applyEvent (State xs) x = State (xs ++ [x])

instance Command State Event Cmd where
    exec _ c =
        case c of
            RegisterValue x -> Right [ValueRegistered x]

instance ToJSON Event
instance ToJSON State
