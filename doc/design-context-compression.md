---
description: Context compression techniques for LLM context windows
---

# Context Compression Skill

## When to Use
- Large strategy knowledge bases need to be passed to LLM
- Summarizing market history for AI analysis
- Reducing token count while preserving signal

## Techniques

### 1. Hierarchical Summarization
- Summarize low-level details first
- Aggregate summaries into higher-level concepts
- Keep only top-level summary in context

### 2. Schema-based Compression
- Define strict schemas for data
- Remove redundant keys
- Use abbreviations for common fields

### 3. Rolling Window
- Keep only recent N items
- Expire old context automatically
- Age-based weighting

## Swimmy Application
- Compress strategy list before sending to AI advisors
- Summarize daily trading logs
- Reduce market data history
