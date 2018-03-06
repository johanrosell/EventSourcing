{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Aggregate                  (AggregateId)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.IORef
import           EventStore                 (EventStore (..), Result,
                                             StateStore (..), handleCommand)
import           InMemory
import           Person
import           Data.Time                  (UTCTime, getCurrentTime)

main :: IO ()
main = do

    -- create in-memory storage for events and states
    eventsRef <- newIORef []
    statesRef <- newIORef []
    let personEvents = mkInMemoryEventStore eventsRef :: EventStore Event
    let personStates = mkInMemoryStateStore statesRef :: StateStore Person

    -- create the command handler for Person events
    let handlePersonCommand = handleCommand personStates personEvents :: (AggregateId, UTCTime, Cmd) -> Result ()

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
    putStrLn $ "Events: "  ++ show events
