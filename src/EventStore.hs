module EventStore (EventStore(..), StateStore(..), Result, handleCommand, errorResult) where

import           Aggregate                  (Aggregate (..), AggregateId,
                                             Command (exec), Envelope (..))
import           Control.Monad.Trans.Except (ExceptT (ExceptT))
import           Data.DateTime              (getCurrentTime)
import           Data.Time                  (UTCTime)

type Result = ExceptT String IO

errorResult :: String -> Result a
errorResult s = ExceptT $ return (Left s)

data EventStore e = EventStore
     { insert        :: Envelope [e] -> Result ()
     , loadEvents    :: String -> Result [Envelope e]
     , loadAllEvents :: () -> Result [Envelope e] }

data StateStore s = StateStore
     { loadState     :: String -> Result (Maybe (Envelope s))
     , loadAllStates :: () -> Result [Envelope s]
     , updateState   :: Envelope s -> Result () }

handleCommand :: (Command s e c) => StateStore s -> EventStore e -> (AggregateId, UTCTime, c) -> Result ()
handleCommand stateStore eventStore (aggrId, d, command) = do
    -- Load the current events from store
    eventEnvelopes <- loadEvents eventStore aggrId
    -- Create a state from the current events
    let (Envelope _ v _ state) =  buildFromZero' eventEnvelopes
    -- Execute the command on the current state
    case exec state command of
        Left err        -> errorResult err
        Right []        -> return ()
        Right newEvents -> do
            -- Store the new events and state
            insert eventStore (Envelope aggrId (v + 1) d newEvents)
            updateState stateStore (Envelope aggrId (v + length newEvents) d (build state newEvents))
