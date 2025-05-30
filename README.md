# Distributed Bank Account System

A sample application demonstrating **formal methods** and **functional programming** principles using Haskell, featuring a distributed bank account system with formal verification.

## Features

### Formal Methods
- **Liquid Haskell annotations** for compile-time verification
- **Invariant checking** (non-negative balances, money conservation)
- **Property-based testing** with QuickCheck
- **Safety properties** (no money creation/destruction)
- **Liveness properties** (transactions eventually complete)

### Functional Programming
- **Immutable data structures** for accounts and transactions
- **Pure functions** for transaction validation and computation
- **Algebraic data types** for modeling transaction types
- **Pattern matching** for handling different scenarios
- **Function composition** for building complex operations

### Concurrency & Distribution
- **Software Transactional Memory (STM)** for thread-safe operations
- **Distributed consensus** using a simple voting protocol
- **Concurrent transaction processing** without deadlocks
- **Node failure handling** and recovery

## Architecture

```
src/
├── BankSystem/
│   ├── Core/
│   │   ├── Account.hs          # Account types and invariants
│   │   ├── Transaction.hs      # Transaction types and validation
│   │   └── Invariants.hs       # System-wide invariants
│   ├── Verification/
│   │   └── Properties.hs       # QuickCheck property tests
│   ├── Concurrent/
│   │   └── STM.hs             # STM-based concurrent operations
│   └── Distributed/
│       ├── Node.hs            # Distributed node implementation
│       └── Consensus.hs       # Simple consensus protocol
```

## Key Invariants

1. **Non-negative balances**: Account balances never go below zero
2. **Money conservation**: Total system money is preserved across transactions
3. **Account consistency**: Accounts maintain valid states
4. **Transaction atomicity**: Operations complete fully or not at all

## Building and Running

```bash
# Build the project
cabal build

# Run the demo
cabal run bank-demo

# Run tests
cabal test

# Run property-based tests
cabal run bank-demo  # includes property testing
```

## Example Usage

The demo application showcases:

1. **Property-based testing** of core invariants
2. **STM operations** with concurrent access
3. **Distributed consensus** for transaction approval
4. **Safety verification** under concurrent load

## Formal Specifications

Key Liquid Haskell specifications:

```haskell
{-@ type NonNegBalance = {v:Balance | v >= 0} @-}
{-@ mkAccount :: AccountId -> NonNegBalance -> IO Account @-}
{-@ applyTransaction :: t:TransactionType -> acc:Account -> 
    {v:Account | (validateTransaction t acc) => (accountInvariant v)} @-}
```

## Educational Value

This project demonstrates:
- How formal methods catch bugs at compile time
- How functional programming enables reasoning about correctness
- How immutability simplifies concurrent programming
- How algebraic types model complex domains precisely
- How property-based testing validates assumptions

## License

MIT License - see LICENSE file for details.