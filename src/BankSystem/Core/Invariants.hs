{-@ LIQUID "--no-termination" @-}

module BankSystem.Core.Invariants
    ( SystemInvariant(..)
    , conservationOfMoney
    , noNegativeBalances
    , accountConsistency
    , transactionConsistency
    , verifySystemInvariants
    ) where

import qualified Data.Map as Map
import BankSystem.Core.Account
import BankSystem.Core.Transaction

data SystemInvariant = SystemInvariant
    { totalSystemBalance :: Balance
    , activeAccounts     :: [Account]
    , completedTxs       :: [Transaction]
    }

{-@ conservationOfMoney :: [Transaction] -> Balance -> Balance -> Bool @-}
conservationOfMoney :: [Transaction] -> Balance -> Balance -> Bool
conservationOfMoney txs initialTotal currentTotal =
    let externalDeposits = sum [amt | tx <- txs, 
                               Deposit amt <- [transactionType tx],
                               status tx == Completed]
        externalWithdrawals = sum [amt | tx <- txs,
                                  Withdrawal amt <- [transactionType tx], 
                                  status tx == Completed]
    in currentTotal == initialTotal + externalDeposits - externalWithdrawals

{-@ noNegativeBalances :: [Account] -> Bool @-}
noNegativeBalances :: [Account] -> Bool
noNegativeBalances = all (\acc -> balance acc >= 0)

{-@ accountConsistency :: [Account] -> Bool @-}
accountConsistency :: [Account] -> Bool
accountConsistency = all accountInvariant

{-@ transactionConsistency :: [Transaction] -> Bool @-}
transactionConsistency :: [Transaction] -> Bool
transactionConsistency = all transactionInvariant

{-@ verifySystemInvariants :: SystemInvariant -> Bool @-}
verifySystemInvariants :: SystemInvariant -> Bool
verifySystemInvariants sys =
    let accounts = activeAccounts sys
        txs = completedTxs sys
        currentTotal = sum (map balance accounts)
    in noNegativeBalances accounts &&
       accountConsistency accounts &&
       transactionConsistency txs &&
       conservationOfMoney txs 0 currentTotal

{-@ assume systemInvariantsHold :: sys:SystemInvariant -> {v:Bool | verifySystemInvariants sys} @-}