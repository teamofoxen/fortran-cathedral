# Fortran Cathedral

> The web, as IBM never intended.

Fortran Cathedral is a real educational website about Fortran, built with an
aggressively unnecessary amount of Fortran. The absurdity is intentional.
The craftsmanship is not.

The site exists and is generated entirely by Fortran: routing, page
assembly, navigation, escaping, the stylesheet and its design tokens, a
rose window placed by sine and cosine, the sitemap, the robots.txt, and
the route manifest. `forty generate` raises it into `dist\`; open
`dist\index.html` in any browser. No server. No JavaScript.

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
forty generate            RAISE THE SITE INTO dist\.
forty validate            SURVEY THE RAISED FABRIC.
forty open                OPEN THE PORCH IN YOUR BROWSER.
forty offer               COMMIT AND PUSH THROUGH FORTY. ONE CONFIRMATION.
forty clean               SWEEP build\ AND dist\. ASKS FIRST.
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

Phase 1 complete: **the smallest real cathedral stands.**

- Phase 0 ordained Forty; the repository was consecrated to GitHub through
  him, and through no other door.
- Phase 1 raised the site: two pages — the Nave and a public Confessional
  whose measurements are taken by Forty at generation time — plus every
  asset, all emitted by Fortran into ignored ground (`dist\`).
- 21 library modules, 1 program, 1 trial file. All Fortran.
- 111 trials passing. 33 validator checks upheld. Deterministic output.
- Executable heresy: 0 lines, enforced by trial and by a validator that
  refuses any script tag in the porch.

Phase 1.1 ordained the Offering Rite: routine commits and pushes now pass
through `forty offer` — inspection, residue refusal, one confirmation,
halt-on-failure, and accord verification, all owned by Fortran. The manual
Phase 1 commit that preceded this rite is permanently recorded in the
ledger's operational chapter and displayed in the public Confessional.

Further wings — the Book of BLAS, the Hall of Deprecated Syntax, the
Saints of Numerical Computing — await their phases.

> THE LANGUAGE IS OLD. THE ARRAYS REMAIN CONTIGUOUS.
