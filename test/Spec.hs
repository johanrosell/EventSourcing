{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Aggregate                  (Aggregate (foldEvents),
                                             AggregateId, Zero (zero))
import           Control.Monad.Trans.Except (runExceptT)
import           Data.IORef
import           Data.Time                  (UTCTime, getCurrentTime)
import           InMemory
import           Store                      (EventStore (..), Result,
                                             StateStore (..), handleCommand)
import           System.Exit
import           Test.QuickCheck
import           Test.QuickCheck.All
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
-- EventStore Properties
-- ####################


-- ####################
-- Helpers
-- ####################

return [] -- strangely, without this QuickCheck will not find the properties
runTests = $quickCheckAll

-- ####################
-- main
-- ####################

main :: IO ()
main = do
  success <- runTests
  if success then exitSuccess
  else exitFailure

{- main = do

    -- create in-memory storage for events and states
    eventsRef <- newIORef []
    statesRef <- newIORef []
    let personEvents = mkInMemoryEventStore eventsRef :: EventStore Event
    let personStates = mkInMemoryStateStore statesRef :: StateStore Person

    -- create the command handler for Person events
    let handlePersonCommand = handleCommand personStates personEvents :: (AggregateId, UTCTime, Cmd) -> EventStore.Result ()

    -- Create a command
    let command = Create Person { name = "Johan", age = 2 }

    -- get the execution time of the command
    date <- getCurrentTime

    -- handle the command
    runExceptT $ handlePersonCommand ("123", date, command)

    -- retrieve the state of the aggregate corresponding to the command
    state <- runExceptT $ loadState personStates "123"

    -- print the state
    case state of
        Right (Just s) -> putStrLn $ "State: " ++ show s
        Right Nothing  -> putStrLn "Error: No state was returned"
        Left err       -> putStrLn err

    -- retrieve and print all events
    events <- runExceptT $ loadAllEvents personEvents ()
    putStrLn $ "Events: "  ++ show events -}
