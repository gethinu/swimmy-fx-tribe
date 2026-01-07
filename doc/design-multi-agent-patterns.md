---
description: Multi-agent architecture patterns
---

# Multi-Agent Patterns Skill

## When to Use
- Designing systems with multiple AI components
- Coordinating Lisp Brain and Rust Body
- Implementing consensus mechanisms

## Patterns

### 1. Dual Monarchy (Current Swimmy Architecture)
- **Lisp**: Sovereign (Constitution, Judgment)
- **Rust**: Executive (Execution, Speed)
- Communication via ZMQ (5555/5556)

### 2. Supervisor Pattern
- One agent monitors others
- Can restart or override failing agents
- Dead Man's Switch implementation

### 3. Ensemble Voting
- Multiple predictors (NN, LSTM, PPO)
- Average or weighted vote
- Confidence-boosted when unanimous

### 4. Specialist Agents
- Each agent handles specific domain
- Router dispatches to appropriate specialist
- Swimmy: Tribes = Specialists

## Swimmy Application
- Dual Monarchy is primary pattern
- Ensemble is used in PREDICT command
- Tribes act as specialist advisors
