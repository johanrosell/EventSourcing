{-# LANGUAGE MultiParamTypeClasses #-}

module Aggregate where

import Data.Time (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar( Day(..) )

type AggregateId = String

zeroDate = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

data Envelope e = Envelope
    { aggregateId :: AggregateId
    , version     :: Int
    , date        :: UTCTime
    , item        :: e }
    deriving (Show)

instance Functor Envelope where
    fmap f (Envelope aid eid date a) = Envelope aid eid date (f a)

-- zero is an initial state of an aggregate before any events have occurred.
class Zero a where
    zero :: a

-- A state representation for the event type 'e'
class (Zero a) => Aggregate a e where
    increment :: a -> e -> a

    build :: a -> [e] -> a
    build = foldl increment

    -- Applies events to the zero state
    buildFromZero :: [e] -> a
    buildFromZero = foldl increment zero

    -- Applies an enveloped event to an enveloped state
    increment'' :: Envelope a -> Envelope e -> Envelope a
    increment'' (Envelope _ v _ state) (Envelope aid _ d event) =
        Envelope aid (v + 1) d (increment state event)

    -- Applies enveloped events to the zero state
    buildFromZero' :: [Envelope e] -> Envelope a
    buildFromZero' =
        foldl increment'' (Envelope "" 0 zeroDate zero)

class (Aggregate a e) => Command a e c where
    exec :: a -> c -> Either String [e]
