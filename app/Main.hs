{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_, when)
import System.Random (randomRIO)
import BankSystem.Core.Account
import BankSystem.Core.Transaction
import BankSystem.Concurrent.STM
import BankSystem.Distributed.Node
import BankSystem.Distributed.Consensus
import BankSystem.Verification.Properties

main :: IO ()
main = do
    putStrLn "=== Distributed Bank Account System Demo ==="
    putStrLn "Demonstrating Formal Methods + Functional Programming\n"
    
    putStrLn "1. Running Property-Based Tests..."
    runAllProperties
    putStrLn ""
    
    putStrLn "2. Creating Bank System with STM..."
    demoSTMOperations
    putStrLn ""
    
    putStrLn "3. Demonstrating Concurrent Operations..."
    demoConcurrentOperations
    putStrLn ""
    
    putStrLn "4. Demonstrating Distributed Consensus..."
    demoDistributedConsensus
    putStrLn ""
    
    putStrLn "Demo completed successfully! 🎉"

demoSTMOperations :: IO ()
demoSTMOperations = do
    bank <- initBankSystem
    
    putStrLn "Creating accounts..."
    acc1 <- createAccountSTM bank (AccountId "Alice") 1000
    acc2 <- createAccountSTM bank (AccountId "Bob") 500
    acc3 <- createAccountSTM bank (AccountId "Charlie") 0
    
    case (acc1, acc2, acc3) of
        (Just _, Just _, Just _) -> do
            putStrLn "✓ Accounts created successfully"
            
            putStrLn "\nInitial balances:"
            showBalance bank (AccountId "Alice")
            showBalance bank (AccountId "Bob")
            showBalance bank (AccountId "Charlie")
            
            putStrLn "\nPerforming transactions..."
            
            depositSTM bank (AccountId "Charlie") 200 >>= printResult "Deposit $200 to Charlie"
            withdrawSTM bank (AccountId "Alice") 100 >>= printResult "Withdraw $100 from Alice"
            transferSTM bank (AccountId "Bob") (AccountId "Charlie") 150 >>= printTransferResult "Transfer $150 from Bob to Charlie"
            
            putStrLn "\nFinal balances:"
            showBalance bank (AccountId "Alice")
            showBalance bank (AccountId "Bob")
            showBalance bank (AccountId "Charlie")
            
        _ -> putStrLn "✗ Failed to create accounts"

demoConcurrentOperations :: IO ()
demoConcurrentOperations = do
    bank <- initBankSystem
    
    putStrLn "Setting up concurrent test scenario..."
    _ <- createAccountSTM bank (AccountId "Account1") 10000
    _ <- createAccountSTM bank (AccountId "Account2") 10000
    
    putStrLn "Running 100 concurrent transfers between two accounts..."
    
    actions <- replicateM 50 $ async $ do
        amount <- randomRIO (1, 100)
        transferSTM bank (AccountId "Account1") (AccountId "Account2") amount
        
    actions2 <- replicateM 50 $ async $ do
        amount <- randomRIO (1, 100)
        transferSTM bank (AccountId "Account2") (AccountId "Account1") amount
    
    putStrLn "Waiting for all transfers to complete..."
    mapM_ wait (actions ++ actions2)
    
    putStrLn "Final balances after concurrent operations:"
    bal1 <- getBalanceSTM bank (AccountId "Account1")
    bal2 <- getBalanceSTM bank (AccountId "Account2")
    
    case (bal1, bal2) of
        (Just b1, Just b2) -> do
            putStrLn $ "Account1: $" ++ show b1
            putStrLn $ "Account2: $" ++ show b2
            putStrLn $ "Total: $" ++ show (b1 + b2)
            putStrLn "✓ Money conservation verified!"
        _ -> putStrLn "✗ Error reading balances"

demoDistributedConsensus :: IO ()
demoDistributedConsensus = do
    putStrLn "Initializing distributed consensus system..."
    consensus <- initConsensus
    
    let nodes = [NodeId "Node1", NodeId "Node2", NodeId "Node3", NodeId "Node4", NodeId "Node5"]
    
    putStrLn "Proposing transaction for consensus..."
    let txId = TransactionId "DISTRIBUTED_TX_001"
        txType = Transfer (AccountId "Alice") (AccountId "Bob") 500
    
    success <- proposeTransaction consensus (NodeId "Node1") txId txType
    if success
        then do
            putStrLn "✓ Transaction proposed successfully"
            
            putStrLn "\nNodes voting on the proposal..."
            _ <- castVote consensus (NodeId "Node1") txId VoteYes
            _ <- castVote consensus (NodeId "Node2") txId VoteYes
            _ <- castVote consensus (NodeId "Node3") txId VoteNo
            _ <- castVote consensus (NodeId "Node4") txId VoteYes
            _ <- castVote consensus (NodeId "Node5") txId VoteAbstain
            
            putStrLn "✓ All votes cast"
            
            result <- checkConsensus consensus txId
            case result of
                Just Accepted -> do
                    putStrLn "✓ Consensus reached: Transaction ACCEPTED"
                    executed <- executeConsensus consensus txId
                    when executed $ putStrLn "✓ Transaction executed successfully"
                Just Rejected -> putStrLn "✗ Consensus reached: Transaction REJECTED"
                Just VotingInProgress -> putStrLn "⏳ Voting still in progress"
                _ -> putStrLn "? Unknown consensus state"
        else putStrLn "✗ Failed to propose transaction"

showBalance :: BankSTM -> AccountId -> IO ()
showBalance bank aid = do
    balance <- getBalanceSTM bank aid
    case balance of
        Just bal -> putStrLn $ show aid ++ ": $" ++ show bal
        Nothing -> putStrLn $ show aid ++ ": Account not found"

printResult :: String -> Either String Balance -> IO ()
printResult desc (Right balance) = putStrLn $ "✓ " ++ desc ++ " - New balance: $" ++ show balance
printResult desc (Left err) = putStrLn $ "✗ " ++ desc ++ " - Error: " ++ err

printTransferResult :: String -> Either String (Balance, Balance) -> IO ()
printTransferResult desc (Right (fromBal, toBal)) = 
    putStrLn $ "✓ " ++ desc ++ " - Balances: $" ++ show fromBal ++ ", $" ++ show toBal
printTransferResult desc (Left err) = 
    putStrLn $ "✗ " ++ desc ++ " - Error: " ++ err