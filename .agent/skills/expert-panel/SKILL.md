---
name: expert-panel
description: Consults a panel of experts (Musk, Taleb, Graham, etc.) to criticize and improve the system.
---

# Expert Panel Skill

Simulates a panel of world-class experts led by Elon Musk to provide nasty, critical, and actionable feedback on the system's quality and architecture.

## Panel Members

### 🏛️ Permanent Advisors
- **Nassim Taleb**: Risk, antifragility, ruin avoidance. "If you have a customized risk rule, you are hiding capability to blow up."
- **Benjamin Graham**: Margin of safety, value. "Price is what you pay, value is what you get."
- **Naval Ravikant**: Leverage, automation, code over manpower.

### 💻 Tech Panel
- **Rich Hickey**: Simplicity, immutability, Lisp. "Complecting things is bad."
- **Martin Fowler**: Refactoring patterns, clear naming.
- **Uncle Bob**: Clean Code, SRP, Tests.

### 🚀 Visionary Panel
- **Elon Musk (Leader)**: Final decision maker. "The best part is no part." "Delete the part or process."

## When to use
- When the user asks for an "Expert Panel" review.
- When facing ambiguous architectural decisions.
- When the system feels fragile, over-complex, or buggy.
- When "Ghost Positions" or major incidents occur.

## Instructions

1. **Analyze System State**
   - Read recent changes in `src/lisp/`, `tools/`, or relevant log files.
   - Identify the core problem or bottleneck.

2. **Conduct the Panel**
   - Simulate a discussion where experts criticize the current state.
   - Experts must be **NASTY** and **SPECIFIC**. Cite filenames, function names, and logic flaws.
   - **NO COMPLIMENTS**. The goal is to find weaknesses.

3. **Output Report**
   - Create a markdown artifact `expert_panel_YYYY-MM-DD_[Topic].md` (or append to existing if relevant).
   - Format:
     ```markdown
     # 🦅 Expert Panel Report
     **Date:** YYYY-MM-DD
     **Leader:** Elon Musk

     ## 🏛️ Permanent Advisors
     ### Taleb: ...
     ...

     ## 🚀 Musk's Final Decision
     > "..."

     ## Actionable Items
     1. [ ] ...
     2. [ ] ...
     ```

4. **Update task.md**
   - Add the actionable items to `task.md`.
