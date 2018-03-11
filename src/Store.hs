{-# LANGUAGE MultiParamTypeClasses #-}

 module Store where

import           Aggregate                  (Aggregate (..), AggregateId,
                                             Command (exec), Zero (zero))
import           Control.Monad.Trans.Except (ExceptT (ExceptT))
import           Data.DateTime              (getCurrentTime)
import           Data.Time                  (UTCTime)
import           Envelope                   (Envelope (..))

type Result = ExceptT String IO

errorResult :: String -> Result a
errorResult x = ExceptT $ return (Left x)

data EventStore e = EventStore
     { insert        :: Envelope [e] -> Result ()
     , loadEvents    :: String -> Result [Envelope e]
     , loadAllEvents :: Result [Envelope e] }

data StateStore s = StateStore
     { loadState     :: String -> Result (Maybe (Envelope s))
     , loadAllStates :: Result [Envelope s]
     , updateState   :: Envelope s -> Result () }

handleCommand :: (Command s e c) => StateStore s -> EventStore e -> (AggregateId, UTCTime, c) -> Result ()
handleCommand (StateStore loadState _ update) (EventStore insert load _) (aggrId, d, command) = do
    -- Load the current events from store
    eventEnvelopes <- load aggrId
    -- Build up a state by folding the events onto the zero state
    let (Envelope _ v _ state) = foldEvents zero eventEnvelopes
    -- Execute the command on the current state
    case exec state command of
        Left err        -> errorResult err
        Right []        -> return ()
        Right newEvents -> do
            -- Store new events and state
            insert (Envelope aggrId (v + 1) d newEvents)
            update (Envelope aggrId (v + length newEvents) d (foldEvents state newEvents))
