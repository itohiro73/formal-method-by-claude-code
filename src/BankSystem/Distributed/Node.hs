{-# LANGUAGE DeriveGeneric #-}

module BankSystem.Distributed.Node
    ( NodeId(..)
    , NodeState(..)
    , DistributedNode(..)
    , NodeMessage(..)
    , initNode
    , sendMessage
    , receiveMessage
    , broadcastMessage
    , nodeInvariant
    ) where

import GHC.Generics (Generic)
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)
import BankSystem.Core.Account
import BankSystem.Core.Transaction
import BankSystem.Concurrent.STM
import Data.Time (UTCTime, getCurrentTime)
import Control.Concurrent.Async
import Network.Socket (HostName, PortNumber)

newtype NodeId = NodeId String
    deriving (Eq, Ord, Show, Generic)

data NodeState
    = NodeActive
    | NodeSuspected
    | NodeFailed
    deriving (Eq, Show, Generic)

data NodeMessage
    = TransactionRequest TransactionId TransactionType
    | TransactionCommit TransactionId
    | TransactionAbort TransactionId String
    | BalanceQuery AccountId
    | BalanceResponse AccountId Balance
    | HeartBeat UTCTime
    | NodeJoin NodeId
    | NodeLeave NodeId
    deriving (Show, Generic)

data DistributedNode = DistributedNode
    { nodeId          :: NodeId
    , nodeState       :: TVar NodeState
    , bankSystem      :: BankSTM
    , peers           :: TVar (Map NodeId NodeState)
    , messageQueue    :: TVar [NodeMessage]
    , isLeader        :: TVar Bool
    , lastHeartbeat   :: TVar UTCTime
    } deriving Generic

initNode :: NodeId -> IO DistributedNode
initNode nid = do
    now <- getCurrentTime
    atomically $ do
        state <- newTVar NodeActive
        bank <- unsafeIOToSTM initBankSystem
        peerMap <- newTVar Map.empty
        msgQueue <- newTVar []
        leader <- newTVar False
        heartbeat <- newTVar now
        return $ DistributedNode nid state bank peerMap msgQueue leader heartbeat

sendMessage :: DistributedNode -> NodeId -> NodeMessage -> IO Bool
sendMessage node targetId msg = do
    peerMap <- readTVarIO (peers node)
    case Map.lookup targetId peerMap of
        Nothing -> return False
        Just NodeFailed -> return False
        _ -> do
            putStrLn $ "Sending message to " ++ show targetId ++ ": " ++ show msg
            return True

receiveMessage :: DistributedNode -> IO (Maybe NodeMessage)
receiveMessage node = atomically $ do
    queue <- readTVar (messageQueue node)
    case queue of
        [] -> return Nothing
        (msg:rest) -> do
            writeTVar (messageQueue node) rest
            return (Just msg)

broadcastMessage :: DistributedNode -> NodeMessage -> IO Int
broadcastMessage node msg = do
    peerMap <- readTVarIO (peers node)
    let activePeers = Map.keys $ Map.filter (/= NodeFailed) peerMap
    results <- mapM (\peerId -> sendMessage node peerId msg) activePeers
    return $ length $ filter id results

handleMessage :: DistributedNode -> NodeMessage -> IO ()
handleMessage node msg = case msg of
    TransactionRequest txId ttype -> do
        putStrLn $ "Processing transaction request: " ++ show txId
        
    TransactionCommit txId -> do
        putStrLn $ "Committing transaction: " ++ show txId
        
    TransactionAbort txId reason -> do
        putStrLn $ "Aborting transaction " ++ show txId ++ ": " ++ reason
        
    BalanceQuery aid -> do
        balance <- getBalanceSTM (bankSystem node) aid
        let response = BalanceResponse aid (maybe 0 id balance)
        putStrLn $ "Balance query result: " ++ show response
        
    BalanceResponse aid balance -> do
        putStrLn $ "Received balance: " ++ show aid ++ " -> " ++ show balance
        
    HeartBeat timestamp -> do
        atomically $ writeTVar (lastHeartbeat node) timestamp
        putStrLn $ "Heartbeat received at " ++ show timestamp
        
    NodeJoin newNodeId -> do
        atomically $ do
            peerMap <- readTVar (peers node)
            writeTVar (peers node) (Map.insert newNodeId NodeActive peerMap)
        putStrLn $ "Node joined: " ++ show newNodeId
        
    NodeLeave leavingNodeId -> do
        atomically $ do
            peerMap <- readTVar (peers node)
            writeTVar (peers node) (Map.delete leavingNodeId peerMap)
        putStrLn $ "Node left: " ++ show leavingNodeId

processMessages :: DistributedNode -> IO ()
processMessages node = do
    maybeMsg <- receiveMessage node
    case maybeMsg of
        Nothing -> return ()
        Just msg -> do
            handleMessage node msg
            processMessages node

nodeInvariant :: DistributedNode -> IO Bool
nodeInvariant node = do
    state <- readTVarIO (nodeState node)
    isLeaderNode <- readTVarIO (isLeader node)
    peerMap <- readTVarIO (peers node)
    return $ case state of
        NodeFailed -> not isLeaderNode
        _ -> True