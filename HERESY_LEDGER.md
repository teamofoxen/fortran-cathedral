# Heresy Ledger

This document records executable non-Fortran logic used by Fortran Cathedral.

Forty should eventually generate or verify the quantitative fields in this ledger.

## Classification rules

Not heresy:

- HTML output,
- CSS,
- Markdown content,
- generated JSON, YAML, XML, SVG, or text,
- Fortran source,
- compiler and linker binaries,
- Git,
- the GitHub CLI,
- the browser,
- the hosting platform.

Potential executable heresy:

- JavaScript,
- TypeScript,
- Python,
- Ruby,
- handwritten C or C++ wrappers,
- shell scripts containing project logic,
- conventional web frameworks,
- external generators that perform application work.

## Current ledger

| File or component | Language | Executable lines | Purpose | Why unavoidable | Removal path |
|---|---:|---:|---|---|---|
| None | — | 0 | — | — | — |

## Operational transgressions

Operational and architectural transgressions are events, not code. They are
recorded here permanently, verified by `forty confess`, displayed in the
public Confessional, and never alter the executable-heresy line count.

| Date | Event | Commit | Executable non-Fortran lines introduced | Why it violated the Cathedral | Remediation | Status |
|---|---|---|---:|---|---|---|
| 2026-07-18 | Phase 1 commit and push performed through direct Git commands rather than Forty | `d2c9f0be63f28b7ecf136c1b9b81a7bd993132db` | 0 | Forty did not yet own routine offerings, but the operation proceeded manually instead of pausing to ordain that capability | Phase 1.1 introduces `forty offer`; all future routine commits and pushes pass through Forty | EXPIATED, NOT ERASED. |

## Expiation record

The stain of the manual Phase 1 offering has been expiated by restitution.
Nothing was erased, amended, squashed, or rewritten; the rite is forward-only.

| Field | Value |
|---|---|
| Expiated transgression | `d2c9f0be63f28b7ecf136c1b9b81a7bd993132db` |
| Withdrawal commit | `c0a39214a655b4d1530e53dfecb4998c01527806` |
| Canonical re-offering commit | `510285b6736fe9c7fa021a3a715b0f3468cb6ce5` |
| Means | Forward-only withdrawal and re-offering formed with git commit-tree; main advanced atomically with git update-ref and pushed without force |
| History | The original transgression remains permanently in history |

## Rules

Every new executable impurity must be added here in the same change that introduces it.

A vague justification such as “easier,” “standard,” or “best practice” is insufficient.

The preferred removal path is always one of:

- replace with Fortran,
- have Forty generate it,
- reduce it to declarative data,
- or prove that it is a genuine platform boundary.
