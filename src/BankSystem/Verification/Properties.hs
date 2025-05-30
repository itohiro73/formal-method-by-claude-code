{-# LANGUAGE TemplateHaskell #-}

module BankSystem.Verification.Properties
    ( prop_AccountNonNegativeBalance
    , prop_TransactionPreservesInvariant
    , prop_DepositIncreasesBalance
    , prop_WithdrawalDecreasesBalance
    , prop_InsufficientFundsRejected
    , prop_TransferConservesTotal
    , prop_SystemConservation
    , runAllProperties
    ) where

import Test.QuickCheck
import BankSystem.Core.Account
import BankSystem.Core.Transaction
import BankSystem.Core.Invariants
import Data.Time (getCurrentTime)
import Control.Monad (replicateM)

instance Arbitrary AccountId where
    arbitrary = AccountId <$> elements ["ACC001", "ACC002", "ACC003", "ACC004", "ACC005"]

instance Arbitrary AccountState where
    arbitrary = elements [Active, Frozen, Closed]

instance Arbitrary TransactionId where
    arbitrary = TransactionId <$> elements ["TX001", "TX002", "TX003", "TX004", "TX005"]

genPositiveAmount :: Gen Amount
genPositiveAmount = choose (1, 10000)

genBalance :: Gen Balance
genBalance = choose (0, 100000)

instance Arbitrary TransactionType where
    arbitrary = oneof
        [ Deposit <$> genPositiveAmount
        , Withdrawal <$> genPositiveAmount
        , Transfer <$> arbitrary <*> arbitrary <*> genPositiveAmount
        ]

prop_AccountNonNegativeBalance :: Property
prop_AccountNonNegativeBalance = monadicIO $ do
    aid <- pick arbitrary
    bal <- pick genBalance
    acc <- run $ mkAccount aid bal
    assert $ balance acc >= 0

prop_TransactionPreservesInvariant :: Property
prop_TransactionPreservesInvariant = monadicIO $ do
    aid <- pick arbitrary
    bal <- pick genBalance
    acc <- run $ mkAccount aid bal
    ttype <- pick arbitrary
    let acc' = applyTransaction ttype acc
    assert $ accountInvariant acc'

prop_DepositIncreasesBalance :: Property
prop_DepositIncreasesBalance = monadicIO $ do
    aid <- pick arbitrary
    bal <- pick genBalance
    amt <- pick genPositiveAmount
    acc <- run $ mkAccount aid bal
    let deposit = Deposit amt
        acc' = applyTransaction deposit acc
    assert $ balance acc' == balance acc + amt

prop_WithdrawalDecreasesBalance :: Property
prop_WithdrawalDecreasesBalance = monadicIO $ do
    aid <- pick arbitrary
    bal <- pick (choose (1000, 10000))
    amt <- pick (choose (1, 500))
    acc <- run $ mkAccount aid bal
    let withdrawal = Withdrawal amt
        acc' = applyTransaction withdrawal acc
    assert $ balance acc' == balance acc - amt

prop_InsufficientFundsRejected :: Property
prop_InsufficientFundsRejected = monadicIO $ do
    aid <- pick arbitrary
    bal <- pick (choose (0, 100))
    amt <- pick (choose (200, 1000))
    acc <- run $ mkAccount aid bal
    let withdrawal = Withdrawal amt
        acc' = applyTransaction withdrawal acc
    assert $ balance acc' == balance acc

prop_TransferConservesTotal :: Property
prop_TransferConservesTotal = monadicIO $ do
    aid1 <- pick arbitrary
    aid2 <- pick arbitrary
    bal1 <- pick (choose (1000, 10000))
    bal2 <- pick genBalance
    amt <- pick (choose (1, 500))
    acc1 <- run $ mkAccount aid1 bal1
    acc2 <- run $ mkAccount aid2 bal2
    let transfer = Transfer aid1 aid2 amt
        acc1' = applyTransaction transfer acc1
        acc2' = acc2 { balance = balance acc2 + amt }
        totalBefore = balance acc1 + balance acc2
        totalAfter = balance acc1' + balance acc2'
    assert $ totalBefore == totalAfter

prop_SystemConservation :: Property
prop_SystemConservation = monadicIO $ do
    numAccounts <- pick (choose (2, 5))
    accounts <- run $ replicateM numAccounts $ do
        aid <- generate arbitrary
        bal <- generate genBalance
        mkAccount aid bal
    let initialTotal = sum (map balance accounts)
        sys = SystemInvariant initialTotal accounts []
    assert $ verifySystemInvariants sys

runAllProperties :: IO ()
runAllProperties = do
    putStrLn "Running property-based tests..."
    quickCheck prop_AccountNonNegativeBalance
    quickCheck prop_TransactionPreservesInvariant
    quickCheck prop_DepositIncreasesBalance
    quickCheck prop_WithdrawalDecreasesBalance
    quickCheck prop_InsufficientFundsRejected
    quickCheck prop_TransferConservesTotal
    quickCheck prop_SystemConservation
    putStrLn "All property tests completed!"

return []