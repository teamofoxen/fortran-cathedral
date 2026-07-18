# Opening Prompt — Fortran Cathedral

You are beginning work on **Fortran Cathedral**.

This project is not asking whether Fortran is the sensible tool. It is asking how far disciplined engineers can push Fortran while still producing a real, polished website.

Before doing anything else:

1. Read `CLAUDE.md` in full.
2. Read `VISION.md`, `BUILD_RULES.md`, `FORTY.md`, and `HERESY_LEDGER.md` in full.
3. Inspect the repository without changing files.
4. Do not initialize Git, connect GitHub, install a normal web stack, or scaffold a conventional application.
5. Report back with:
   - your understanding of the product,
   - the architectural premise,
   - the Dumbness Mandate,
   - the boundaries around non-Fortran executable code,
   - your proposed **Phase 0: Ordain Forty**,
   - how Forty will safely drive Git and GitHub setup,
   - the smallest credible Phase 1 website slice after Forty exists,
   - the exact files you propose to create or modify,
   - every expected impurity or external tool boundary.
6. Stop and wait for authorization before implementation.

## The project

Fortran Cathedral is a real, polished, educational website about Fortran, built with an aggressively unnecessary amount of Fortran.

The absurdity is intentional. The craftsmanship is not.

This is not a parody repository containing a few `.f90` files while a conventional web stack does the real work. The architecture, build machinery, generated assets, tests, repository setup, and public Confessional must honor the premise.

Fortran should do not only the serious work but the dumb work:

- generate pages,
- own routing and content assembly,
- emit trivial assets,
- count heresy,
- validate content,
- prepare deployments,
- and, through Forty, drive the GitHub connection setup.

The browser may require HTML and CSS. A small amount of JavaScript may eventually be genuinely unavoidable. Such code must be minimized, isolated, justified, and entered in the Heresy Ledger.

## Forty

Forty is a real Fortran CLI and the operational front door of the Cathedral.

The first build phase must establish Forty before the website architecture expands.

GitHub connection setup must be initiated and supervised by Forty. Forty may invoke official `git` and `gh` tools, but it must own the checks, prompts, sequencing, exit-status handling, verification, and final report. Forty must never capture or store credentials.

Do not manually perform the normal GitHub setup behind Forty's back because it would be faster.

## Product character

The site should feel like:

- a Gothic cathedral built inside an old scientific computing terminal,
- reverent toward numerical computing,
- funny because it is played completely straight,
- genuinely useful to someone curious about Fortran,
- visually distinctive without becoming unreadable,
- and architecturally committed beyond all reasonable expectation.

Possible content and experiences include:

- why Fortran still exists,
- old versus modern Fortran,
- famous numerical libraries,
- weather, aerospace, physics, and supercomputing use cases,
- the Book of BLAS,
- Saints of Numerical Computing,
- the Hall of Deprecated Syntax,
- the Confessional,
- the Verger's Room showing Forty and current build facts,
- interactive or simulated code exhibits.

## Prime directive

> Use Fortran for every component it can plausibly perform safely, including trivial, awkward, ceremonial, infrastructural, and comically inappropriate work. When two approaches are viable, prefer the one that gives Fortran the more unnecessary job. Do not reinterpret this into a conventional stack with Fortran branding.

Do not begin implementation until I authorize Build.
