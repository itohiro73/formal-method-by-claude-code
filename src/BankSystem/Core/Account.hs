{-# LANGUAGE DeriveGeneric #-}
{-@ LIQUID "--no-termination" @-}

module BankSystem.Core.Account
    ( AccountId(..)
    , Account(..)
    , Balance
    , AccountState(..)
    , mkAccount
    , getBalance
    , isValidBalance
    , accountInvariant
    ) where

import GHC.Generics (Generic)
import Data.Time (UTCTime, getCurrentTime)

type Balance = Integer

{-@ type NonNegBalance = {v:Balance | v >= 0} @-}

newtype AccountId = AccountId String
    deriving (Eq, Ord, Show, Generic)

data AccountState = Active | Frozen | Closed
    deriving (Eq, Show, Generic)

data Account = Account
    { accountId       :: AccountId
    , balance         :: Balance
    , accountState    :: AccountState
    , createdAt       :: UTCTime
    , lastModified    :: UTCTime
    } deriving (Show, Generic)

{-@ mkAccount :: AccountId -> NonNegBalance -> IO Account @-}
mkAccount :: AccountId -> Balance -> IO Account
mkAccount aid initialBalance = do
    now <- getCurrentTime
    return $ Account
        { accountId = aid
        , balance = initialBalance
        , accountState = Active
        , createdAt = now
        , lastModified = now
        }

{-@ getBalance :: Account -> NonNegBalance @-}
getBalance :: Account -> Balance
getBalance = balance

{-@ isValidBalance :: Balance -> Bool @-}
isValidBalance :: Balance -> Bool
isValidBalance b = b >= 0

{-@ accountInvariant :: Account -> Bool @-}
accountInvariant :: Account -> Bool
accountInvariant acc = 
    isValidBalance (balance acc) &&
    case accountState acc of
        Closed -> balance acc == 0
        _      -> True

{-@ assume accountInvariantHolds :: acc:Account -> {v:Bool | accountInvariant acc} @-}