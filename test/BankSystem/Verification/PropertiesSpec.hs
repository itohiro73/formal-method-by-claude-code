module BankSystem.Verification.PropertiesSpec (propertiesSpecs) where

import Test.Hspec
import Test.QuickCheck
import BankSystem.Verification.Properties

propertiesSpecs :: Spec
propertiesSpecs = do
    describe "Property-based tests" $ do
        it "accounts always have non-negative balances" $ 
            property prop_AccountNonNegativeBalance
            
        it "transactions preserve account invariants" $ 
            property prop_TransactionPreservesInvariant
            
        it "deposits increase balance" $ 
            property prop_DepositIncreasesBalance
            
        it "withdrawals decrease balance" $ 
            property prop_WithdrawalDecreasesBalance
            
        it "insufficient funds are rejected" $ 
            property prop_InsufficientFundsRejected
            
        it "transfers conserve total money" $ 
            property prop_TransferConservesTotal
            
        it "system maintains conservation" $ 
            property prop_SystemConservation