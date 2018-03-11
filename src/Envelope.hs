{-# LANGUAGE MultiParamTypeClasses #-}

module Envelope where

import           Aggregate          (Aggregate (applyEvent), AggregateId,
                                     Zero (zero))
import           Data.Time          (UTCTime (UTCTime), secondsToDiffTime)
import           Data.Time.Calendar (Day (ModifiedJulianDay))

-- | Types

data Envelope a = Envelope
    { aggregateId :: String
    , version     :: Int
    , date        :: UTCTime
    , item        :: a }
    deriving (Show)

-- | Instances

instance (Aggregate e s) => Aggregate (Envelope e) (Envelope s) where
  applyEvent (Envelope aid _ _ s) (Envelope _ v d e) = Envelope aid v d (applyEvent s e)

instance (Zero a) => Zero (Envelope a) where
  zero = Envelope "" 0 zero zero
