# Fortran Cathedral

> The web, as IBM never intended.

Fortran Cathedral is a real educational website about Fortran, built with an
aggressively unnecessary amount of Fortran. The absurdity is intentional.
The craftsmanship is not.

The website does not exist yet. Its operator does. This is by design:
**Phase 0 ordains the verger before the first stone of the nave is laid.**

## Forty

Forty is the Cathedral's operational front door: a real command-line program
written in Fortran, compiled by `fpm`, owning the checks, sequencing,
verification, and proclamations for every project operation.

```text
forty help                THIS PROCLAMATION.
forty version             THE VERGER'S VERSION.
forty doctor              EXAMINE COMPILER, FPM, GIT, AND GH.
forty status              THE STATE OF THE CATHEDRAL.
forty build               COMPILE VIA FPM. BLESS A STAMP.
forty test                RUN THE TRIALS VIA FPM.
forty confess             MEASURE HERESY. AUDIT THE LEDGER.
forty clean               SWEEP build\. ASKS FIRST.
forty github status       REPORT THE GATEHOUSE.
forty github connect      THE CONSECRATION RITE. ONE CONFIRMATION.
forty github verify       CONFIRM THE REMOTE IS CANONICAL.
```

## Getting started

Requirements: GFortran and `fpm` (on Windows, MSYS2's UCRT64 packages serve
well), plus `git` and the GitHub CLI `gh` for repository rites.

The one permitted bootstrap:

```text
fpm run forty -- doctor
```

Thereafter, think in Forty. `forty build` places an ordained copy of the
binary at a stable path, `build\bin\forty.exe`, so the faithful need not
memorize fpm's hashed corridors.

One Windows nuance, disclosed honestly: a running executable cannot be
replaced. When Forty's own scripture changes, rebuild via
`fpm run forty -- build` — the verger cannot rebuild himself while standing
in the doorway. For the same reason, `forty clean` may leave the verger's
own shoes in the swept yard; sweep again from outside.

## The GitHub rite

`forty github connect` drives the repository's introduction to GitHub. It
states the complete sequence of intended actions — init, `.gitignore`
inscription, first commit, `gh repo create --push`, verification — and asks
for **one** confirmation covering the whole transaction. It halts at the
first failure and reports plainly. `--dry-run` shows the rite without
performing it.

Forty never requests, reads, echoes, or stores credentials. The GitHub CLI
keeps the keys; Forty owns the absurdly solemn orchestration.

## Purity

All executable logic in this repository is Fortran. HTML, CSS, Markdown,
TOML, and other declarative artifacts are honest inputs and outputs, not
heresy. Unavoidable non-Fortran executable code, should it ever arrive, will
be minimized, isolated, and recorded in [HERESY_LEDGER.md](HERESY_LEDGER.md).

Current measurement, by Forty himself: **0 executable lines of heresy.**

## The canon

Read, in order: [CLAUDE.md](CLAUDE.md), [VISION.md](VISION.md),
[BUILD_RULES.md](BUILD_RULES.md), [FORTY.md](FORTY.md),
[HERESY_LEDGER.md](HERESY_LEDGER.md), [OPENING_PROMPT.md](OPENING_PROMPT.md).

## Status

Phase 0 complete: **Forty is ordained.**

- 12 library modules, 1 program, 1 trial file. All Fortran.
- 83 trials passing.
- Git: uninitiated. The ground awaits consecration through
  `forty github connect`, and through no other door.

Phase 1 — the smallest real cathedral: a Fortran static-site generator, three
pages, and a Confessional rendered from Forty's own measurements — awaits
authorization.

> THE LANGUAGE IS OLD. THE ARRAYS REMAIN CONTIGUOUS.
