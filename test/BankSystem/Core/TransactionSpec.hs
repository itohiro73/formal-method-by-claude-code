module BankSystem.Core.TransactionSpec (transactionSpecs) where

import Test.Hspec
import BankSystem.Core.Account
import BankSystem.Core.Transaction

transactionSpecs :: Spec
transactionSpecs = do
    describe "Transaction creation" $ do
        it "creates deposit transaction" $ do
            tx <- mkTransaction (TransactionId "TX1") (Deposit 100)
            transactionInvariant tx `shouldBe` True
            
        it "creates withdrawal transaction" $ do
            tx <- mkTransaction (TransactionId "TX2") (Withdrawal 50)
            transactionInvariant tx `shouldBe` True
    
    describe "Transaction validation" $ do
        it "validates deposit with active account" $ do
            acc <- mkAccount (AccountId "TEST") 1000
            validateTransaction (Deposit 100) acc `shouldBe` True
            
        it "validates withdrawal with sufficient funds" $ do
            acc <- mkAccount (AccountId "TEST") 1000
            validateTransaction (Withdrawal 500) acc `shouldBe` True
            
        it "rejects withdrawal with insufficient funds" $ do
            acc <- mkAccount (AccountId "TEST") 100
            validateTransaction (Withdrawal 500) acc `shouldBe` False
    
    describe "Transaction application" $ do
        it "applies deposit correctly" $ do
            acc <- mkAccount (AccountId "TEST") 1000
            let acc' = applyTransaction (Deposit 100) acc
            getBalance acc' `shouldBe` 1100
            
        it "applies withdrawal correctly" $ do
            acc <- mkAccount (AccountId "TEST") 1000
            let acc' = applyTransaction (Withdrawal 100) acc
            getBalance acc' `shouldBe` 900
            
        it "preserves account invariants" $ do
            acc <- mkAccount (AccountId "TEST") 1000
            let acc' = applyTransaction (Deposit 100) acc
            accountInvariant acc' `shouldBe` True