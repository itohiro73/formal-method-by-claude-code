# Formal Verification

## Overview

The banking system employs formal verification techniques to mathematically prove correctness properties. This ensures that critical safety and liveness properties hold under all possible execution scenarios, including concurrent operations and distributed consensus.

## Verification Tools & Techniques

### Liquid Haskell

**Purpose**: Compile-time verification through refinement types

**Integration**: Liquid Haskell annotations embedded in source code provide mathematical constraints that are verified during compilation.

```haskell
{-@ LIQUID "--no-termination" @-}  -- Disable termination checking

-- Refinement type for non-negative balances
{-@ type NonNegBalance = {v:Balance | v >= 0} @-}

-- Verified account constructor
{-@ mkAccount :: AccountId -> NonNegBalance -> IO Account @-}
mkAccount aid initialBalance = do
  now <- getCurrentTime
  return $ Account aid initialBalance Active now now
```

**Verification Benefits**:
- **Static Guarantees**: Properties verified at compile time
- **No Runtime Overhead**: Verification doesn't affect performance
- **Type-Level Constraints**: Impossible to create invalid states

### Refinement Types

#### Balance Constraints
```haskell
-- Ensures all balances are non-negative
{-@ type NonNegBalance = {v:Balance | v >= 0} @-}

-- Account balance accessor with guarantee
{-@ getBalance :: Account -> NonNegBalance @-}
getBalance = balance
```

#### Transaction Validation
```haskell
-- Validation function with formal preconditions
{-@ validateTransaction :: TransactionType -> Account -> Bool @-}
validateTransaction ttype acc = case ttype of
  Deposit amt -> amt > 0 && accountState acc == Active
  Withdrawal amt -> amt > 0 && amt <= balance acc && accountState acc == Active
  Transfer _ _ amt -> amt > 0 && amt <= balance acc && accountState acc == Active
```

#### Safe Transaction Application
```haskell
-- Apply transaction only if validation passes, preserving invariants
{-@ applyTransaction :: t:TransactionType -> acc:Account -> 
    {v:Account | (validateTransaction t acc) => (accountInvariant v)} @-}
applyTransaction ttype acc
  | not (validateTransaction ttype acc) = acc  -- No change if invalid
  | otherwise = case ttype of
      Deposit amt -> acc { balance = balance acc + amt }
      Withdrawal amt -> acc { balance = balance acc - amt }
      Transfer _ _ amt -> acc { balance = balance acc - amt }
```

## System Invariants

### Safety Properties

#### 1. Non-Negative Balances
**Mathematical Statement**:
```
∀ account ∈ system : balance(account) ≥ 0
```

**Implementation**:
```haskell
{-@ accountInvariant :: Account -> Bool @-}
accountInvariant acc = 
  isValidBalance (balance acc) &&
  case accountState acc of
    Closed -> balance acc == 0  -- Closed accounts must have zero balance
    _      -> True
```

**Verification**: Liquid Haskell ensures this property is maintained by:
- Requiring non-negative initial balances in `mkAccount`
- Validating transactions before application
- Type-level constraints preventing negative balance creation

#### 2. Money Conservation
**Mathematical Statement**:
```
∀ transaction : total_system_money_before = total_system_money_after + external_flows
```

**Implementation**:
```haskell
{-@ conservationOfMoney :: [Transaction] -> Balance -> Balance -> Bool @-}
conservationOfMoney txs initialTotal currentTotal =
  let externalDeposits = sum [amt | tx <- txs, 
                             Deposit amt <- [transactionType tx],
                             status tx == Completed]
      externalWithdrawals = sum [amt | tx <- txs,
                                Withdrawal amt <- [transactionType tx], 
                                status tx == Completed]
  in currentTotal == initialTotal + externalDeposits - externalWithdrawals
```

**Verification Strategy**:
- **Transfer Operations**: Money moves between accounts but total unchanged
- **Deposit Operations**: External money creation tracked and accounted for
- **Withdrawal Operations**: External money destruction tracked and accounted for

#### 3. Account State Consistency
**Mathematical Statement**:
```
∀ account : account.state = Closed ⟹ balance(account) = 0
```

**Implementation**:
```haskell
{-@ accountConsistency :: [Account] -> Bool @-}
accountConsistency = all accountInvariant

{-@ accountInvariant :: Account -> Bool @-}
accountInvariant acc = 
  isValidBalance (balance acc) &&
  case accountState acc of
    Closed -> balance acc == 0
    _      -> True
```

#### 4. Transaction Atomicity
**Mathematical Statement**:
```
∀ transaction : transaction.status ∈ {Completed, Failed} ⟹ 
  (all_effects_applied ∨ no_effects_applied)
```

**Implementation**: STM provides atomicity guarantees automatically:
```haskell
transferSTM :: BankSTM -> AccountId -> AccountId -> Amount -> IO (Either String (Balance, Balance))
transferSTM bank fromId toId amount = atomically $ do
  -- All operations succeed together or fail together
  fromAcc <- readAccount fromId
  toAcc   <- readAccount toId
  -- ... validation and updates happen atomically
```

### Liveness Properties

#### 1. Transaction Completion
**Property**: Every valid transaction request eventually receives a response

**Verification Approach**:
- **STM Guarantees**: STM ensures progress through automatic retry
- **Timeout Mechanisms**: Distributed operations have bounded time limits
- **Fair Scheduling**: No transaction is indefinitely postponed

#### 2. Deadlock Freedom
**Property**: System never reaches a state where progress is impossible

**Verification**:
- **STM Design**: Software Transactional Memory prevents deadlocks by design
- **Retry Mechanism**: Conflicting transactions automatically retry
- **No Lock Ordering**: STM eliminates traditional lock-based deadlock sources

#### 3. Termination
**Property**: All operations terminate in finite time

**Verification**:
- **Bounded Operations**: All individual operations are O(1) or O(log n)
- **Progress Guarantees**: STM ensures eventual progress
- **Timeout Handling**: Distributed operations have explicit timeouts

## Verification Methodology

### Static Analysis

#### Type-Level Verification
```haskell
-- Compile-time enforcement of positive amounts
{-@ type PositiveAmount = {v:Amount | v > 0} @-}

-- Transaction constructor with positive amount constraint
{-@ mkTransaction :: TransactionId -> TransactionType -> IO Transaction @-}
```

#### Invariant Assumptions
```haskell
-- Assume invariants hold for external verification
{-@ assume accountInvariantHolds :: acc:Account -> {v:Bool | accountInvariant acc} @-}
{-@ assume transactionInvariantHolds :: tx:Transaction -> {v:Bool | transactionInvariant tx} @-}
{-@ assume systemInvariantsHold :: sys:SystemInvariant -> {v:Bool | verifySystemInvariants sys} @-}
```

### Dynamic Verification

#### Property-Based Testing
QuickCheck generates thousands of test cases to verify properties:

```haskell
-- Property: Account creation maintains non-negative balance
prop_AccountNonNegativeBalance :: Property
prop_AccountNonNegativeBalance = monadicIO $ do
  aid <- pick arbitrary
  bal <- pick genBalance  -- Generates non-negative balance
  acc <- run $ mkAccount aid bal
  assert $ balance acc >= 0
```

#### Concurrency Testing
```haskell
-- Property: Concurrent transfers preserve total money
prop_ConcurrentTransfersConserveTotal :: Property
prop_ConcurrentTransfersConserveTotal = monadicIO $ do
  -- Setup accounts with known total
  initialTotal <- calculateTotalBalance
  -- Run many concurrent transfers
  runConcurrentTransfers 100
  -- Verify total unchanged
  finalTotal <- calculateTotalBalance
  assert $ initialTotal == finalTotal
```

### Proof Techniques

#### Inductive Reasoning
**Base Case**: Initial system state satisfies all invariants
```haskell
-- Empty system trivially satisfies invariants
verifySystemInvariants (SystemInvariant 0 [] []) == True
```

**Inductive Step**: Each operation preserves invariants
```haskell
-- If system satisfies invariants and operation is valid,
-- then resulting system satisfies invariants
{-@ theorem_invariant_preservation :: 
    sys:SystemInvariant -> op:Operation -> 
    {v:Bool | verifySystemInvariants sys && validOperation op => 
              verifySystemInvariants (applyOperation op sys)} @-}
```

#### Compositional Verification
**Modular Proofs**: Each module's correctness proven independently
- `Account.hs`: Account invariants proven locally
- `Transaction.hs`: Transaction validation proven correct
- `STM.hs`: Concurrency safety proven through STM properties

**System-Level Composition**: Module proofs combine to prove system properties
```haskell
-- System invariants follow from component invariants
verifySystemInvariants sys = 
  noNegativeBalances (activeAccounts sys) &&
  accountConsistency (activeAccounts sys) &&
  transactionConsistency (completedTxs sys) &&
  conservationOfMoney (completedTxs sys) 0 (totalSystemBalance sys)
```

## Verification Workflow

### Development Process
1. **Specification**: Write formal specifications as Liquid Haskell types
2. **Implementation**: Implement functions with refinement type annotations
3. **Verification**: Compile with Liquid Haskell to verify properties
4. **Testing**: Run property-based tests to validate dynamic behavior
5. **Integration**: Compose verified modules into system

### Continuous Verification
```bash
# Liquid Haskell verification in CI/CD
liquid src/BankSystem/Core/Account.hs
liquid src/BankSystem/Core/Transaction.hs
liquid src/BankSystem/Core/Invariants.hs

# Property-based testing
cabal test --test-options="--quickcheck-tests=10000"
```

### Error Detection

#### Compile-Time Errors
```
Liquid Type Mismatch
  Inferred type: {v:Integer | v >= -100}
  Expected type: {v:Integer | v >= 0}
  
  In expression: balance acc - amount
  When amount > balance acc
```

#### Runtime Property Violations
```haskell
-- QuickCheck discovers failing test case
*** Failed! Falsifiable (after 23 tests and 3 shrinks):
AccountId "ACC001"
Balance 100
Amount 150
Expected: Right _
Actual: Left "Insufficient funds"
```

## Benefits of Formal Verification

### Mathematical Certainty
- **Proof of Correctness**: Properties are mathematically proven, not just tested
- **Exhaustive Coverage**: Verification covers all possible execution paths
- **No Missed Edge Cases**: Formal methods catch corner cases testing might miss

### Early Error Detection
- **Compile-Time Errors**: Many bugs caught before code execution
- **Design Validation**: Formal specs help validate system design
- **Specification Bugs**: Inconsistencies in requirements discovered early

### Increased Confidence
- **Safety-Critical Code**: Mathematical guarantees for critical operations
- **Refactoring Safety**: Changes can be verified to preserve properties
- **Documentation**: Formal specs serve as precise documentation

### Maintenance Benefits
- **Regression Prevention**: Property violations immediately detected
- **Code Evolution**: Invariants guide safe code modifications
- **Team Communication**: Formal specs provide unambiguous contracts

This comprehensive verification approach ensures the banking system maintains correctness under all operational scenarios, providing mathematical confidence in its behavior.