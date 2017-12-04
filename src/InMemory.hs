{-# LANGUAGE MultiParamTypeClasses #-}

module InMemory (mkInMemoryEventStore, mkInMemoryStateStore) where

import           Aggregate                 (Envelope (Envelope, aggregateId, version))
import           Control.Monad.Trans.Class (lift)
import           Data.Function             (on)
import           Data.IORef
import           Data.List                 (maximumBy)
import           EventStore                (EventStore (EventStore), Result,
                                            StateStore (StateStore),
                                            errorResult)

getAll :: IORef [Envelope a] -> Result [Envelope a]
getAll ioRef = do
    xs <- lift $ readIORef ioRef
    return xs

getForId :: IORef [Envelope a] -> String -> Result [Envelope a]
getForId ioRef aggrId = do
    xs <- lift $ readIORef ioRef
    return $ filter (\x -> aggregateId x == aggrId) xs

find :: IORef [Envelope a] -> String -> Result (Maybe (Envelope a))
find ioRef aggrId = do
    xs <- lift $ readIORef ioRef
    case filter (\x -> aggregateId x == aggrId) xs of
        []  -> return Nothing
        x:_ -> return $ Just x

insert :: IORef [Envelope a] -> Envelope [a] -> Result ()
insert ref (Envelope aid v events) = do
    currentEvents <- getForId ref aid
    let currentVersion = case currentEvents of
                            [] -> 0
                            xs -> version (maximumBy (compare `on` version) xs)
    case v == currentVersion + 1 of
        False -> errorResult $ "Expected current version of aggregate '" ++ aid ++ "' to be '" ++ (show $ v + 1) ++ "', but it was '" ++ (show currentVersion) ++ "'"
        True -> return ()
    -- Put events in separate envelopes with distinct versions
    let envelopes = mapWithIndex (\x i -> Envelope aid (v + i) x) events
    lift $ modifyIORef ref (\xs -> xs ++ envelopes)

update :: IORef [Envelope a] -> Envelope a -> Result ()
update ref state = do
    xs <- lift $ readIORef ref
    let xs' = state:(filter (\x -> aggregateId x /= aggregateId state) xs)
    lift $ writeIORef ref xs'
    return ()

mkInMemoryEventStore :: IORef [Envelope e] -> EventStore e
mkInMemoryEventStore events =
    EventStore
        (insert events)
        (getForId events)
        (\_ -> getAll events)

mkInMemoryStateStore :: IORef [Envelope s] -> StateStore s
mkInMemoryStateStore states =
    StateStore
        (find states)
        (\_ -> getAll states)
        (update states)

-- helpers

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f l = zipWith f l [0..]
