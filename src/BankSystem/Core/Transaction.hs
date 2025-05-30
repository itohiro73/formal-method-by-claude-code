{-# LANGUAGE DeriveGeneric #-}
{-@ LIQUID "--no-termination" @-}

module BankSystem.Core.Transaction
    ( TransactionId(..)
    , Transaction(..)
    , TransactionType(..)
    , TransactionStatus(..)
    , Amount
    , mkTransaction
    , validateTransaction
    , applyTransaction
    , transactionInvariant
    ) where

import GHC.Generics (Generic)
import Data.Time (UTCTime, getCurrentTime)
import BankSystem.Core.Account

type Amount = Integer

{-@ type PositiveAmount = {v:Amount | v > 0} @-}

newtype TransactionId = TransactionId String
    deriving (Eq, Ord, Show, Generic)

data TransactionType
    = Deposit Amount
    | Withdrawal Amount  
    | Transfer AccountId AccountId Amount
    deriving (Show, Generic)

data TransactionStatus
    = Pending
    | Completed
    | Failed String
    | Cancelled
    deriving (Show, Generic)

data Transaction = Transaction
    { transactionId     :: TransactionId
    , transactionType   :: TransactionType
    , status            :: TransactionStatus
    , timestamp         :: UTCTime
    , sourceAccount     :: Maybe AccountId
    , targetAccount     :: Maybe AccountId
    } deriving (Show, Generic)

{-@ mkTransaction :: TransactionId -> TransactionType -> IO Transaction @-}
mkTransaction :: TransactionId -> TransactionType -> IO Transaction
mkTransaction tid ttype = do
    now <- getCurrentTime
    let (source, target) = extractAccounts ttype
    return $ Transaction
        { transactionId = tid
        , transactionType = ttype
        , status = Pending
        , timestamp = now
        , sourceAccount = source
        , targetAccount = target
        }

extractAccounts :: TransactionType -> (Maybe AccountId, Maybe AccountId)
extractAccounts (Deposit _) = (Nothing, Nothing)
extractAccounts (Withdrawal _) = (Nothing, Nothing)
extractAccounts (Transfer from to _) = (Just from, Just to)

{-@ validateTransaction :: TransactionType -> Account -> Bool @-}
validateTransaction :: TransactionType -> Account -> Bool
validateTransaction ttype acc =
    case ttype of
        Deposit amt -> amt > 0 && accountState acc == Active
        Withdrawal amt -> amt > 0 && amt <= balance acc && accountState acc == Active
        Transfer _ _ amt -> amt > 0 && amt <= balance acc && accountState acc == Active

{-@ applyTransaction :: t:TransactionType -> acc:Account -> 
    {v:Account | (validateTransaction t acc) => (accountInvariant v)} @-}
applyTransaction :: TransactionType -> Account -> Account
applyTransaction ttype acc
    | not (validateTransaction ttype acc) = acc
    | otherwise = case ttype of
        Deposit amt -> acc { balance = balance acc + amt }
        Withdrawal amt -> acc { balance = balance acc - amt }
        Transfer _ _ amt -> acc { balance = balance acc - amt }

{-@ transactionInvariant :: Transaction -> Bool @-}
transactionInvariant :: Transaction -> Bool
transactionInvariant tx =
    case transactionType tx of
        Deposit amt -> amt > 0
        Withdrawal amt -> amt > 0
        Transfer _ _ amt -> amt > 0

{-@ assume transactionInvariantHolds :: tx:Transaction -> {v:Bool | transactionInvariant tx} @-}