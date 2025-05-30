{-# LANGUAGE DeriveGeneric #-}

module BankSystem.Distributed.Consensus
    ( ConsensusState(..)
    , Proposal(..)
    , Vote(..)
    , ConsensusResult(..)
    , SimpleConsensus(..)
    , initConsensus
    , proposeTransaction
    , castVote
    , checkConsensus
    , executeConsensus
    ) where

import GHC.Generics (Generic)
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)
import BankSystem.Core.Transaction
import BankSystem.Distributed.Node
import Data.Time (UTCTime, getCurrentTime)

data Vote = VoteYes | VoteNo | VoteAbstain
    deriving (Eq, Show, Generic)

data Proposal = Proposal
    { proposalId    :: TransactionId
    , proposedTx    :: TransactionType
    , proposer      :: NodeId
    , timestamp     :: UTCTime
    , requiredVotes :: Int
    } deriving (Show, Generic)

data ConsensusState
    = Proposed
    | VotingInProgress
    | Accepted
    | Rejected
    | Executed
    deriving (Eq, Show, Generic)

data ConsensusResult = ConsensusResult
    { proposal      :: Proposal
    , votes         :: Map NodeId Vote
    , finalState    :: ConsensusState
    , yesVotes      :: Int
    , noVotes       :: Int
    , abstainVotes  :: Int
    } deriving (Show, Generic)

data SimpleConsensus = SimpleConsensus
    { activeProposals :: TVar (Map TransactionId ConsensusResult)
    , nodeList        :: TVar [NodeId]
    , consensusHistory :: TVar [ConsensusResult]
    } deriving Generic

initConsensus :: IO SimpleConsensus
initConsensus = atomically $ do
    proposals <- newTVar Map.empty
    nodes <- newTVar []
    history <- newTVar []
    return $ SimpleConsensus proposals nodes history

proposeTransaction :: SimpleConsensus -> NodeId -> TransactionId -> TransactionType -> IO Bool
proposeTransaction consensus proposerNode txId txType = do
    now <- getCurrentTime
    atomically $ do
        proposals <- readTVar (activeProposals consensus)
        nodes <- readTVar (nodeList consensus)
        if Map.member txId proposals
            then return False
            else do
                let proposal = Proposal txId txType proposerNode now (length nodes)
                    result = ConsensusResult proposal Map.empty Proposed 0 0 0
                writeTVar (activeProposals consensus) (Map.insert txId result proposals)
                return True

castVote :: SimpleConsensus -> NodeId -> TransactionId -> Vote -> IO Bool
castVote consensus voter txId vote = atomically $ do
    proposals <- readTVar (activeProposals consensus)
    case Map.lookup txId proposals of
        Nothing -> return False
        Just result -> 
            if finalState result `elem` [Accepted, Rejected, Executed]
                then return False
                else do
                    let newVotes = Map.insert voter vote (votes result)
                        (yesCount, noCount, abstainCount) = countVotes newVotes
                        newResult = result 
                            { votes = newVotes
                            , yesVotes = yesCount
                            , noVotes = noCount
                            , abstainVotes = abstainCount
                            }
                    writeTVar (activeProposals consensus) (Map.insert txId newResult proposals)
                    return True

countVotes :: Map NodeId Vote -> (Int, Int, Int)
countVotes voteMap = 
    let voteList = Map.elems voteMap
        yesCount = length $ filter (== VoteYes) voteList
        noCount = length $ filter (== VoteNo) voteList
        abstainCount = length $ filter (== VoteAbstain) voteList
    in (yesCount, noCount, abstainCount)

checkConsensus :: SimpleConsensus -> TransactionId -> IO (Maybe ConsensusState)
checkConsensus consensus txId = atomically $ do
    proposals <- readTVar (activeProposals consensus)
    case Map.lookup txId proposals of
        Nothing -> return Nothing
        Just result -> do
            let totalVotes = yesVotes result + noVotes result + abstainVotes result
                requiredVotes' = requiredVotes (proposal result)
                majorityThreshold = (requiredVotes' `div` 2) + 1
                
            newState <- if totalVotes < requiredVotes'
                then return VotingInProgress
                else if yesVotes result >= majorityThreshold
                    then return Accepted
                    else return Rejected
                    
            let updatedResult = result { finalState = newState }
            writeTVar (activeProposals consensus) (Map.insert txId updatedResult proposals)
            return (Just newState)

executeConsensus :: SimpleConsensus -> TransactionId -> IO Bool
executeConsensus consensus txId = atomically $ do
    proposals <- readTVar (activeProposals consensus)
    case Map.lookup txId proposals of
        Nothing -> return False
        Just result ->
            if finalState result == Accepted
                then do
                    let executedResult = result { finalState = Executed }
                    writeTVar (activeProposals consensus) (Map.insert txId executedResult proposals)
                    
                    history <- readTVar (consensusHistory consensus)
                    writeTVar (consensusHistory consensus) (executedResult : history)
                    
                    return True
                else return False

consensusInvariant :: SimpleConsensus -> IO Bool
consensusInvariant consensus = do
    proposals <- readTVarIO (activeProposals consensus)
    let results = Map.elems proposals
        validStates = all (\r -> finalState r `elem` [Proposed, VotingInProgress, Accepted, Rejected, Executed]) results
        validVoteCounts = all (\r -> yesVotes r + noVotes r + abstainVotes r == Map.size (votes r)) results
    return $ validStates && validVoteCounts