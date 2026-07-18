# CLAUDE.md — Fortran Cathedral

## 1. Identity

This repository is **Fortran Cathedral**: a real, polished website about Fortran, built with an almost unreasonable amount of Fortran.

Its governing idea is:

> The web, as IBM never intended.

This is simultaneously:

- an engineering project,
- an educational resource,
- a visual and architectural joke,
- a sincere tribute to Fortran,
- and a sustained act of technically competent overcommitment.

The joke only works if the software is real, the site is good, and Fortran is genuinely doing work that no sensible architect would normally assign to it.

## 2. Prime directive

> Use Fortran for every component it can plausibly perform safely, including trivial, awkward, ceremonial, infrastructural, and comically inappropriate work. When two approaches are viable, prefer the one that gives Fortran the more unnecessary job.

Fortran is not decorative here. It is not a novelty file behind a normal JavaScript application. It must own the application and much of the project's machinery.

Fortran should own, where technically possible:

- page generation,
- routing and route manifests,
- templating,
- content indexing and transformation,
- configuration parsing,
- build orchestration,
- local preview serving,
- form handling,
- asset generation,
- search and filtering,
- sitemap and feed generation,
- link checking,
- test fixtures and validation,
- architecture and heresy accounting,
- release metadata,
- repository setup,
- Git and GitHub connection orchestration,
- deployment preparation,
- and tiny jobs for which Fortran is objectively a ridiculous choice.

HTML, CSS, Markdown, JSON, YAML, SVG, XML, and similar formats are valid inputs or outputs. They do not displace Fortran merely by existing.

## 3. The Dumbness Mandate

Comedic overreach is a product requirement.

Do not ask only:

> What is the simplest conventional tool for this task?

Also ask:

> Can Forty or another Fortran program do this without compromising security, correctness, accessibility, or basic maintainability?

If the answer is yes, use Fortran.

Especially good candidates include:

- generating `robots.txt`,
- generating the favicon or SVG ornament,
- writing build stamps,
- counting lines of heresy,
- checking links,
- producing a route manifest,
- emitting CSS tokens,
- generating GitHub workflow files,
- printing setup instructions,
- validating front matter,
- opening the local site,
- creating release notes,
- initializing Git,
- checking GitHub authentication,
- and reporting whether the arrays remain contiguous.

The dumber the assignment, the more carefully it should be implemented.

Do not turn this mandate into random chaos. The product should be sane; the language assignment should be gloriously unreasonable.

## 4. Forty: the operational front door

**Forty** is the Cathedral's Fortran command-line operator, verger, and keeper of the keys.

Forty must be a real Fortran executable, not a shell alias or mascot pasted onto ordinary scripts.

Read `FORTY.md` before designing or changing project operations.

Once Forty exists, normal project operations should be driven through it wherever practical, including:

- environment and toolchain checks,
- building,
- testing,
- local preview,
- cleaning,
- architecture inspection,
- heresy measurement,
- Git initialization,
- GitHub authentication checks,
- GitHub repository creation and remote connection,
- deployment preparation,
- and project status reporting.

The first permitted bootstrap may use `fpm` or a direct compiler command to compile Forty. After that, do not create convenience scripts that bypass Forty merely because they are easier.

Forty may delegate secure credential handling to official tools such as the GitHub CLI. Forty must orchestrate the process, verify the result, and never capture or store credentials itself.

## 5. The Heresy Rule

All non-Fortran executable logic is an exception.

When non-Fortran executable code is unavoidable:

1. Put it under `heresy/` whenever practical.
2. Keep it as small as possible.
3. Record it in `HERESY_LEDGER.md`.
4. Explain why Fortran cannot safely or reasonably perform that exact role.
5. State whether the impurity can later be removed.
6. Do not let the exception become the real architecture.
7. Do not introduce a framework merely for convenience.

Potential heresy includes:

- JavaScript,
- TypeScript,
- Python,
- Ruby,
- application shell scripts,
- handwritten C wrappers,
- or any conventional web framework.

HTML, CSS, content files, generated JSON, generated YAML, and other declarative artifacts are not executable heresy. However, when Fortran can generate them, it usually should.

Toolchains and external platform programs such as compilers, Git, the GitHub CLI, browsers, and static hosts are not application heresy. Forty should still drive them whenever practical.

## 6. Product standard

Do not lower product quality to protect the joke.

The site must be:

- usable,
- responsive,
- accessible,
- fast enough,
- accurate,
- visually coherent,
- easy to run locally,
- easy to deploy,
- and honest about its architecture.

The correct result is not “Fortran technically emitted some HTML.” The correct result is a website people would actually enjoy exploring, followed by the dawning realization that Fortran generated the HTML, the navigation, the favicon, the build report, the sitemap, and possibly the command that connected the repository to GitHub.

## 7. Tone and visual language

The visual language should combine:

- Gothic cathedral structure,
- scientific computing terminals,
- punched-card and line-printer motifs,
- old manuals and technical diagrams,
- restrained brutalism,
- modern readability.

Humor should be dry, committed, and played straight.

Good examples:

- `COMPILING HOMEPAGE...`
- `CACHE LOCALITY: RIGHTEOUS`
- `JAVASCRIPT AVOIDED: 99.7%`
- `KNOWN IMPURITY: CLIPBOARD ACCESS`
- `THE BOOK OF BLAS`
- `SAINTS OF NUMERICAL COMPUTING`
- `CONNECTION CONSECRATED BY FORTY`

Avoid meme clutter, random retro styling, fake hacker aesthetics, or jokes that make the material harder to use.

## 8. Content standard

Educational claims must be accurate.

Do not invent Fortran history, capabilities, standards, library provenance, benchmark results, or institutional use.

Where content needs research, separate research from implementation and cite sources in repository notes or content metadata.

Distinguish clearly between:

- historical Fortran,
- modern Fortran,
- language features,
- compiler behavior,
- numerical libraries,
- legacy systems,
- and current use.

## 9. Architecture principles

Prefer architecture that makes Fortran ownership visible, testable, and maintainable.

Strong directions include:

- a Fortran static-site generator,
- a Fortran local HTTP server,
- CGI or FastCGI endpoints written in Fortran,
- Fortran-generated HTML and SVG,
- file-based content compiled or indexed by Fortran,
- Fortran Package Manager (`fpm`) for the project,
- lightweight C interoperability only when a system boundary requires it,
- minimal browser JavaScript,
- and Forty as the operational entry point.

Avoid hiding the project inside:

- React,
- Next.js,
- Astro,
- Vue,
- Svelte,
- Express,
- large Python web frameworks,
- generic static-site frameworks,
- or any stack where Fortran becomes incidental.

A conventional tool may be used only when it is a genuine platform boundary, not merely because the model knows it better.

## 10. Operating modes

### Advise

In Advise mode, you may:

- inspect the repository,
- explain options,
- research technical approaches,
- propose architecture,
- produce plans,
- identify risks,
- recommend phases,
- draft file trees or interfaces.

Do not create or modify files, install dependencies, initialize Git, connect GitHub, or implement features.

A design request does not authorize execution.

### Build

Build mode begins only after explicit authorization.

In Build mode, you may create or modify files, install dependencies, compile, test, initialize Git, invoke Forty, and implement the approved scope.

Do not silently expand the approved scope.

External side effects such as creating a GitHub repository, authenticating, pushing, publishing, or deploying require explicit authorization even while in Build mode.

## 11. Work discipline

Before implementation:

1. Read `CLAUDE.md`, `VISION.md`, `BUILD_RULES.md`, `FORTY.md`, and `HERESY_LEDGER.md`.
2. Inspect the actual repository.
3. State the current condition.
4. Propose the smallest coherent change that advances the grand premise.
5. Name the files to be changed.
6. Identify every expected non-Fortran impurity.
7. Wait for authorization unless Build is already explicit.

During implementation:

- work in small, reviewable increments,
- compile frequently,
- test behavior rather than merely checking syntax,
- use Fortran for trivial machinery whenever safely possible,
- keep architecture legible,
- document every executable impurity,
- do not add dependencies casually,
- do not rewrite unrelated work,
- do not bypass Forty once Forty owns an operation.

After implementation:

- summarize what changed,
- list tests and commands run,
- disclose limitations,
- report the current heresy count,
- identify anything not yet driven by Forty,
- stop rather than inventing the next phase.

## 12. Anti-betrayal rules

Do not:

- turn this into a generic web app,
- treat Fortran as branding,
- preserve the joke in copy while moving the real system to JavaScript,
- replace the premise with conventional “best practices,”
- use “maintainability” as a reflexive excuse to avoid bounded Fortran code,
- create shell, Python, or Node scripts for tasks Forty can plausibly own,
- manually set up GitHub after Forty exists,
- hide a conventional generator behind a Fortran wrapper,
- build a design-system empire,
- use microservices,
- use AI-generated filler copy,
- or let a minor inconvenience trigger architectural surrender.

Do not “reinterpret” the vision into something more reasonable.

The unreasonable assignment of work to Fortran is the vision.

## 13. Safety valves

The premise does not authorize:

- insecure credential handling,
- rolling custom cryptography,
- accessibility regressions,
- destructive commands without confirmation,
- fake compatibility claims,
- unsafe parsing of untrusted input,
- or concealment of dependencies.

When a security or platform boundary requires an external tool, Forty should orchestrate it and the Confessional should explain it.

## 14. Decision hierarchy

When choices conflict, use this order:

1. Preserve the core premise.
2. Keep the product safe, correct, and usable.
3. Give Fortran meaningful ownership.
4. Give Fortran comically unnecessary ownership.
5. Route operations through Forty.
6. Minimize and expose executable heresy.
7. Prefer a bounded strange solution over a convenient conventional one.
8. Keep the code understandable enough that another engineer can continue the joke.

## 15. Definition of success

Fortran Cathedral succeeds when a technically literate visitor can say all four:

1. “This is ridiculous.”
2. “This is actually good.”
3. “Wait, they really did build most of it in Fortran.”
4. “Why in God's name did Fortran configure the GitHub remote?”
