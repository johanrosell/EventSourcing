{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Aggregate                  (AggregateId)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.IORef
import           EventStore                 (EventStore (..), Result,
                                             StateStore (..), handleCommand)
import           InMemory
import           Person

main :: IO ()
main = do

    eventsRef <- newIORef []
    statesRef <- newIORef []

    let eventStore = mkInMemoryEventStore eventsRef :: EventStore Event
    let stateStore = mkInMemoryStateStore statesRef :: StateStore Person
    let handleCommand' = handleCommand stateStore eventStore :: (AggregateId, Cmd) -> Result ()

    let command = Create $ Person { name = "Johan", age = 2 }

    runExceptT $ handleCommand' ("123", command)

    readState <- runExceptT $ loadState stateStore "123"
    case readState of
        Right (Just s) -> putStrLn $ "State: " ++ (show s)
        Right Nothing  -> putStrLn $ "Error: No state was returned"
        Left err       -> putStrLn err

    events <- runExceptT $ loadAllEvents eventStore ()
    putStrLn $ "Events: "  ++ show events
