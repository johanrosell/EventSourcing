{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Aggregate
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Aeson                 (encode)
import           Data.IORef
import           Data.List                  (head)
import           EventStore                 (Envelope (..), EventStore (..),
                                             Result)
import           InMemoryEventStore
import           Person

main :: IO ()
main = do

    eventsRef <- newIORef []
    statesRef <- newIORef []

    let EventStore { commit = commit
                   , loadEvents = load
                   , loadAllEvents = loadAll
                   , getReadState = getState
                   , getReadStates = getStates
                   , updateReadState = updateState } = mkInMemoryEventStore eventsRef statesRef :: EventStore Event Person

    let events = [ Envelope { aggregateId = "123"
                            , eventId = 1
                            , item = Created $ Person { name = "Johan"
                                                      , age = 37 } } ]

    runExceptT $ commit (head events)
    readState' <- runExceptT $ getState "123"
    case readState' of
        Right (Just s) -> putStrLn $ "State: "-- ++ (show s)
        Left err       -> putStrLn err
    --
    --putStrLn $ "State: " ++ (show $ encode state)
    return ()
