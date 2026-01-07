---
description: Memory systems design for multi-agent architectures
---

# Memory Systems Skill

## When to Use
- Designing persistent memory for trading agents
- Managing state across Lisp/Rust components
- Implementing knowledge bases

## Memory Types

### 1. Working Memory (Short-term)
- Current positions, pending orders
- Recent market data (last 20 candles)
- Active strategy signals

### 2. Episodic Memory (Medium-term)
- Daily trading log
- Session-level performance
- Strategy execution history

### 3. Semantic Memory (Long-term)
- Strategy knowledge base
- Learned market patterns
- Constitution rules

## Swimmy Application
- `*strategy-knowledge-base*` = Semantic Memory
- `*position-history*` = Episodic Memory
- Live market state = Working Memory
