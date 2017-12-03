{-# LANGUAGE MultiParamTypeClasses #-}

module Aggregate where

type AggregateId = String

data Envelope e = Envelope
    { aggregateId :: AggregateId
    , version     :: Word
    , item        :: e }
    deriving (Show)

instance Functor Envelope where
    fmap f (Envelope aid eid a) = Envelope aid eid (f a)

-- zero is an initial state of an aggregate before any events have occurred.
class Zero a where
    zero :: a

-- A state representation for the event type 'e'
class (Zero a) => Aggregate a e where
    increment :: a -> e -> a

    build :: a -> [e] -> a
    build state es = foldl increment state es

    buildFromZero :: [e] -> a
    buildFromZero es = foldl increment zero es

    increment'' :: Envelope a -> Envelope e -> Envelope a
    increment'' (Envelope _ v state) (Envelope aid _ event) =
        (Envelope aid (v+1) (increment state event))

    increment' :: Envelope a -> [e] -> Envelope a
    increment' (Envelope aid v state) events =
        let newState = foldl increment state events
        in Envelope aid (v + (fromIntegral $ length events)) newState

    buildFromZero' :: [Envelope e] -> Envelope a
    buildFromZero' es =
        foldl increment'' (Envelope "" 0 zero) es

class (Aggregate a e) => Command a e c where
    exec :: a -> c -> [e]
