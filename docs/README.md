# Formal Method Banking System Documentation

This documentation provides a comprehensive overview of the distributed banking system implementation that demonstrates formal methods and functional programming principles using Haskell.

## Documentation Structure

### Core Documentation
- [**Application Specification**](specification.md) - Detailed specification of the banking system's requirements and behavior
- [**Internal Architecture**](architecture.md) - System design, module organization, and component interactions
- [**Formal Verification**](formal-verification.md) - How formal methods are used throughout the system
- [**Testing Strategy**](testing.md) - Comprehensive testing approach including property-based testing

### Quick Start
1. **Building**: `cabal build`
2. **Running Demo**: `cabal run bank-demo`
3. **Running Tests**: `cabal test`

## System Overview

The system implements a distributed banking platform with the following key characteristics:

### Formal Methods Integration
- **Liquid Haskell** annotations for compile-time verification
- **Invariant preservation** across all operations
- **Property-based testing** with QuickCheck
- **Mathematical proofs** of safety and liveness properties

### Functional Programming Principles
- **Immutable data structures** for accounts and transactions
- **Pure functions** for all business logic
- **Algebraic data types** for precise domain modeling
- **Compositional design** enabling safe concurrent operations

### Concurrency & Distribution
- **Software Transactional Memory (STM)** for thread-safe operations
- **Distributed consensus** using voting protocols
- **Failure tolerance** and recovery mechanisms
- **Deadlock-free** concurrent transaction processing

## Key Features

| Feature | Description | Module |
|---------|-------------|---------|
| **Account Management** | Thread-safe account creation and balance management | `BankSystem.Core.Account` |
| **Transaction Processing** | Deposits, withdrawals, and transfers with full validation | `BankSystem.Core.Transaction` |
| **Invariant Checking** | System-wide invariant verification | `BankSystem.Core.Invariants` |
| **Concurrent Operations** | STM-based concurrent transaction processing | `BankSystem.Concurrent.STM` |
| **Distributed Consensus** | Multi-node transaction approval | `BankSystem.Distributed.Consensus` |
| **Property Testing** | Automated verification of system properties | `BankSystem.Verification.Properties` |

## Educational Value

This system serves as a practical example of:

1. **Formal Verification in Practice** - Real-world application of Liquid Haskell
2. **Functional Architecture** - Large-scale functional programming design
3. **Concurrent Programming** - Safe concurrency without locks
4. **Distributed Systems** - Consensus protocols and failure handling
5. **Property-Based Testing** - Automated testing of complex invariants

## Dependencies

The system uses several key Haskell libraries:

- **liquidhaskell** - Compile-time verification
- **stm** - Software Transactional Memory
- **QuickCheck** - Property-based testing
- **async** - Asynchronous operations
- **containers** - Efficient data structures

## Navigation

Navigate through the documentation using the links above, or refer to specific modules:

- For **system requirements**: See [Specification](specification.md)
- For **implementation details**: See [Architecture](architecture.md)
- For **verification approach**: See [Formal Verification](formal-verification.md)
- For **testing methodology**: See [Testing](testing.md)