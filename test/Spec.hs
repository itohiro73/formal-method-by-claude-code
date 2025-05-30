module Main where

import Test.Hspec
import BankSystem.Core.AccountSpec
import BankSystem.Core.TransactionSpec
import BankSystem.Verification.PropertiesSpec

main :: IO ()
main = hspec $ do
    describe "BankSystem.Core.Account" accountSpecs
    describe "BankSystem.Core.Transaction" transactionSpecs
    describe "BankSystem.Verification.Properties" propertiesSpecs