{-# LANGUAGE MultiParamTypeClasses #-}

module InMemoryEventStore (mkInMemoryEventStore) where

import           Aggregate                 (Aggregate)
import           Control.Monad.Trans.Class (lift)
import           Data.IORef
import           EventStore                (Envelope (aggregateId),
                                            EventStore (EventStore), Result)

loadAll :: IORef [Envelope a] -> Result [Envelope a]
loadAll ioRef = do
    xs <- lift $ readIORef ioRef
    return xs

load :: IORef [Envelope a] -> String -> Result [Envelope a]
load ioRef aggrId = do
    xs <- lift $ readIORef ioRef
    return $ filter (\x -> aggregateId x == aggrId) xs

find :: IORef [Envelope a] -> String -> Result (Maybe (Envelope a))
find ioRef aggrId = do
    xs <- lift $ readIORef ioRef
    case filter (\x -> aggregateId x == aggrId) xs of
        []  -> return Nothing
        x:_ -> return $ Just x

commit :: IORef [Envelope a] -> Envelope a -> Result ()
commit ref x =
    lift $ modifyIORef ref (\xs -> xs ++ [x])

update :: IORef [Envelope a] -> Envelope a -> Result ()
update ref state = do
    xs <- lift $ readIORef ref
    let xs' = state:(filter (\x -> aggregateId x /= aggregateId state) xs)
    lift $ writeIORef ref xs'
    return ()

mkInMemoryEventStore :: (Aggregate e s) => IORef [Envelope e] -> IORef [Envelope s] -> EventStore e s
mkInMemoryEventStore events states =
    EventStore
        (commit events)
        (load events)
        (\_ -> loadAll events)
        (find states)
        (\_ -> loadAll states)
        (update states)
