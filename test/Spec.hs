{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Aggregate                  (Aggregate (foldEvents),
                                             AggregateId, Zero (zero))
import           Control.Monad.Trans.Except (runExceptT)
import           Data.IORef
import           Data.Time                  (UTCTime, getCurrentTime)
import           Envelope
import           InMemory
import           Store                      (EventStore (..), Result,
                                             StateStore (..), handleCommand)
import           System.Exit
import           Test.QuickCheck
import           Test.QuickCheck.All
import           Test.QuickCheck.Monadic
import           Types

-- ####################
-- Aggregate Properties
-- ####################

prop_all_events_folded_onto_state_in_order :: Int -> Bool
prop_all_events_folded_onto_state_in_order eventCount =
  let events = map ValueRegistered [1..eventCount]
      state = foldEvents zero events :: State
  in eventLog state == events

-- ####################
-- Store Properties
-- ####################

prop_state_store_updated_on_new_event = monadicIO $ do
    -- aggregate ID to execute command against
    let aggregateId = "123"
    -- set up the stores
    (stateStore, eventStore) <- run mkStores
    -- Execute command and check result is OK
    cmdResult <- run $ runExceptT $ handleCommand stateStore eventStore (aggregateId, zero, RegisterValue 2)
    case cmdResult of
      Right _  -> return ()
      Left err -> run $ putStrLn err
    -- Get the state for the aggregate ID of the event
    stateResult <- run $ runExceptT $ loadState stateStore aggregateId
    -- check that the result is OK and that the stored state has the expected value
    case stateResult of
      Left err    -> do run $ putStrLn ("Failed to retrieve state " ++ err)
                        assert False
      Right (Just (Envelope _ _ _ state)) -> assert $ state == State [ValueRegistered 2]

prop_event_store_updated_on_new_event value = monadicIO $ do
    -- aggregate ID to execute command against
    let aggregateId = "123"
    -- set up the stores
    (stateStore, eventStore) <- run mkStores
    -- Execute command and check result is OK
    cmdResult <- run $ runExceptT $ handleCommand stateStore eventStore (aggregateId, zero, RegisterValue value)
    case cmdResult of
      Right _  -> run $ return ()
      Left err -> run $ putStrLn ("Command execution failed " ++ err)
    -- Get the state for the aggregate ID of the event
    eventResult <- run $ runExceptT $ loadAllEvents eventStore
    -- check that the result is OK and that the stored state has the expected value
    case eventResult of
      Left err    -> do run $ putStrLn err
                        assert False
      Right [Envelope _ _ _ event] -> assert $ event == ValueRegistered value

-- ####################
-- Helpers
-- ####################

mkStores :: IO (StateStore State, EventStore Event)
mkStores = do
    stateRef <- newIORef []
    eventRef <- newIORef []
    let stateStore = mkInMemoryStateStore stateRef :: StateStore State
    let eventStore = mkInMemoryEventStore eventRef :: EventStore Event
    return (stateStore, eventStore)

return [] -- strangely, without this QuickCheck will not find the properties
runTests = $quickCheckAll

-- ####################
-- Program entry point
-- ####################

main :: IO ()
main = do
  success <- runTests
  if success then exitSuccess
  else exitFailure
