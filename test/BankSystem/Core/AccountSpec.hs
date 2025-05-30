module BankSystem.Core.AccountSpec (accountSpecs) where

import Test.Hspec
import BankSystem.Core.Account

accountSpecs :: Spec
accountSpecs = do
    describe "Account creation" $ do
        it "creates account with valid balance" $ do
            acc <- mkAccount (AccountId "TEST") 1000
            getBalance acc `shouldBe` 1000
            accountInvariant acc `shouldBe` True
            
        it "creates account with zero balance" $ do
            acc <- mkAccount (AccountId "TEST") 0
            getBalance acc `shouldBe` 0
            accountInvariant acc `shouldBe` True
    
    describe "Balance validation" $ do
        it "validates positive balances" $ do
            isValidBalance 100 `shouldBe` True
            
        it "validates zero balance" $ do
            isValidBalance 0 `shouldBe` True
            
        it "rejects negative balances" $ do
            isValidBalance (-1) `shouldBe` False
    
    describe "Account invariants" $ do
        it "maintains invariants for active accounts" $ do
            acc <- mkAccount (AccountId "TEST") 500
            accountInvariant acc `shouldBe` True