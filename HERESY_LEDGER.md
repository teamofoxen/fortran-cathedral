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

## Rules

Every new executable impurity must be added here in the same change that introduces it.

A vague justification such as “easier,” “standard,” or “best practice” is insufficient.

The preferred removal path is always one of:

- replace with Fortran,
- have Forty generate it,
- reduce it to declarative data,
- or prove that it is a genuine platform boundary.
