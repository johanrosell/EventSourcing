{-# LANGUAGE MultiParamTypeClasses #-}

module EventStore (EventStore(..), StateStore(..), Result, handleCommand) where

import           Aggregate                  (Aggregate (..), AggregateId,
                                             Command (exec), Envelope (..))
import           Control.Monad.Trans.Except (ExceptT)

type Result = ExceptT String IO

data EventStore e = EventStore
     { insert        :: Envelope [e] -> Result ()
     , loadEvents    :: String -> Result [Envelope e]
     , loadAllEvents :: () -> Result [Envelope e] }

data StateStore s = StateStore
     { loadState     :: String -> Result (Maybe (Envelope s))
     , loadAllStates :: () -> Result [Envelope s]
     , updateState   :: Envelope s -> Result () }

handleCommand :: (Command s e c) => StateStore s -> EventStore e -> (AggregateId, c) -> Result ()
handleCommand stateStore eventStore (aggrId, command) = do
    -- Load the current events from store
    eventEnvelopes <- loadEvents eventStore aggrId
    -- Create a state from the current events
    let (Envelope _ v state) =  buildFromZero' eventEnvelopes
    -- Execute the command on the current state
    case exec state command of
        []        -> return ()
        newEvents -> do
            -- Store the new events and state
            insert eventStore (Envelope aggrId (v + 1) newEvents)
            updateState stateStore (Envelope aggrId (v + (fromIntegral $ length newEvents)) (build state newEvents))