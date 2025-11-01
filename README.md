# 🌾 Transparent Subsidy Distribution Platform (Economics/Social Welfare)
A decentralized platform for transparent distribution of agricultural and energy subsidies using Stacks blockchain.

## 🎯 Features

- ✨ Transparent subsidy distribution
- 🔐 Secure beneficiary registration
- 💰 Multiple subsidy types support
- 📊 Real-time distribution tracking
- 🏦 Treasury management
- 📈 Platform statistics

## 🚀 Getting Started

### Prerequisites

- Clarinet
- Stacks wallet

### Contract Functions

#### For Administrators

- `register-beneficiary`: Register eligible beneficiaries
- `add-subsidy-type`: Create new subsidy types
- `fund-treasury`: Add funds to treasury

#### For Users

- `distribute-subsidy`: Claim available subsidy
- `get-beneficiary-info`: View beneficiary details
- `get-subsidy-type-info`: View subsidy type details
- `get-platform-stats`: View platform statistics

## 💡 Usage Example

1. Register a beneficiary:
```clarity
(contract-call? .subsidy-platform register-beneficiary 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM "agriculture")
```

2. Add subsidy type:
```clarity
(contract-call? .subsidy-platform add-subsidy-type "agriculture" u1000 u144)
```

3. Distribute subsidy:
```clarity
(contract-call? .subsidy-platform distribute-subsidy 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

## 🔒 Security

- Only contract owner can register beneficiaries and manage subsidy types
- Built-in checks for eligibility and distribution periods
- Treasury balance validation

## 📝 License

