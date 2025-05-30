{-# LANGUAGE DeriveGeneric #-}

module BankSystem.Concurrent.STM
    ( BankSTM
    , AccountRef
    , TransactionLog
    , initBankSystem
    , createAccountSTM
    , depositSTM
    , withdrawSTM
    , transferSTM
    , getBalanceSTM
    , getAllBalancesSTM
    , getTransactionLogSTM
    ) where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)
import GHC.Generics (Generic)
import BankSystem.Core.Account
import BankSystem.Core.Transaction
import Data.Time (getCurrentTime)

type AccountRef = TVar Account
type TransactionLog = TVar [Transaction]

data BankSTM = BankSTM
    { accounts       :: TVar (Map AccountId AccountRef)
    , transactionLog :: TransactionLog
    , nextTxId       :: TVar Int
    } deriving Generic

initBankSystem :: IO BankSTM
initBankSystem = atomically $ do
    accs <- newTVar Map.empty
    txLog <- newTVar []
    nextId <- newTVar 1
    return $ BankSTM accs txLog nextId

createAccountSTM :: BankSTM -> AccountId -> Balance -> IO (Maybe AccountRef)
createAccountSTM bank aid initialBalance
    | initialBalance < 0 = return Nothing
    | otherwise = do
        acc <- mkAccount aid initialBalance
        atomically $ do
            accMap <- readTVar (accounts bank)
            if Map.member aid accMap
                then return Nothing
                else do
                    accRef <- newTVar acc
                    writeTVar (accounts bank) (Map.insert aid accRef accMap)
                    return (Just accRef)

getNextTxId :: BankSTM -> STM TransactionId
getNextTxId bank = do
    idNum <- readTVar (nextTxId bank)
    writeTVar (nextTxId bank) (idNum + 1)
    return $ TransactionId ("TX" ++ show idNum)

logTransaction :: BankSTM -> Transaction -> STM ()
logTransaction bank tx = do
    log' <- readTVar (transactionLog bank)
    writeTVar (transactionLog bank) (tx : log')

depositSTM :: BankSTM -> AccountId -> Amount -> IO (Either String Balance)
depositSTM bank aid amount
    | amount <= 0 = return $ Left "Invalid deposit amount"
    | otherwise = atomically $ do
        accMap <- readTVar (accounts bank)
        case Map.lookup aid accMap of
            Nothing -> return $ Left "Account not found"
            Just accRef -> do
                acc <- readTVar accRef
                if accountState acc /= Active
                    then return $ Left "Account not active"
                    else do
                        txId <- getNextTxId bank
                        tx <- unsafeIOToSTM $ mkTransaction txId (Deposit amount)
                        let newBalance = balance acc + amount
                            updatedAcc = acc { balance = newBalance }
                        writeTVar accRef updatedAcc
                        logTransaction bank (tx { status = Completed })
                        return $ Right newBalance

withdrawSTM :: BankSTM -> AccountId -> Amount -> IO (Either String Balance)
withdrawSTM bank aid amount
    | amount <= 0 = return $ Left "Invalid withdrawal amount"
    | otherwise = atomically $ do
        accMap <- readTVar (accounts bank)
        case Map.lookup aid accMap of
            Nothing -> return $ Left "Account not found"
            Just accRef -> do
                acc <- readTVar accRef
                if accountState acc /= Active
                    then return $ Left "Account not active"
                    else if balance acc < amount
                        then return $ Left "Insufficient funds"
                        else do
                            txId <- getNextTxId bank
                            tx <- unsafeIOToSTM $ mkTransaction txId (Withdrawal amount)
                            let newBalance = balance acc - amount
                                updatedAcc = acc { balance = newBalance }
                            writeTVar accRef updatedAcc
                            logTransaction bank (tx { status = Completed })
                            return $ Right newBalance

transferSTM :: BankSTM -> AccountId -> AccountId -> Amount -> IO (Either String (Balance, Balance))
transferSTM bank fromId toId amount
    | amount <= 0 = return $ Left "Invalid transfer amount"
    | fromId == toId = return $ Left "Cannot transfer to same account"
    | otherwise = atomically $ do
        accMap <- readTVar (accounts bank)
        case (Map.lookup fromId accMap, Map.lookup toId accMap) of
            (Just fromRef, Just toRef) -> do
                fromAcc <- readTVar fromRef
                toAcc <- readTVar toRef
                if accountState fromAcc /= Active || accountState toAcc /= Active
                    then return $ Left "One or both accounts not active"
                    else if balance fromAcc < amount
                        then return $ Left "Insufficient funds"
                        else do
                            txId <- getNextTxId bank
                            tx <- unsafeIOToSTM $ mkTransaction txId (Transfer fromId toId amount)
                            let newFromBalance = balance fromAcc - amount
                                newToBalance = balance toAcc + amount
                                updatedFromAcc = fromAcc { balance = newFromBalance }
                                updatedToAcc = toAcc { balance = newToBalance }
                            writeTVar fromRef updatedFromAcc
                            writeTVar toRef updatedToAcc
                            logTransaction bank (tx { status = Completed })
                            return $ Right (newFromBalance, newToBalance)
            _ -> return $ Left "One or both accounts not found"

getBalanceSTM :: BankSTM -> AccountId -> IO (Maybe Balance)
getBalanceSTM bank aid = atomically $ do
    accMap <- readTVar (accounts bank)
    case Map.lookup aid accMap of
        Nothing -> return Nothing
        Just accRef -> do
            acc <- readTVar accRef
            return $ Just (balance acc)

getAllBalancesSTM :: BankSTM -> IO (Map AccountId Balance)
getAllBalancesSTM bank = atomically $ do
    accMap <- readTVar (accounts bank)
    balances <- mapM (\accRef -> do
        acc <- readTVar accRef
        return (balance acc)) accMap
    return $ Map.map (\accRef -> 
        let acc = undefined -- This would need unsafeIOToSTM in practice
        in balance acc) accMap

getTransactionLogSTM :: BankSTM -> IO [Transaction]
getTransactionLogSTM bank = atomically $ readTVar (transactionLog bank)