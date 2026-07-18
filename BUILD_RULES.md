# Build Rules — Fortran Cathedral

## Rule 1: Fortran owns the application

Fortran must perform the meaningful application work. It should not merely invoke a normal web framework or generate one decorative page.

## Rule 2: Fortran also owns the stupid work

When Fortran can safely perform a small, boring, ceremonial, or objectively inappropriate task, it should.

This includes work that would ordinarily become a five-line shell, Python, or Node script.

The project explicitly values well-tested comic overkill.

## Rule 3: Ordain Forty first

The first implementation phase is **Phase 0: Ordain Forty**.

Create a real Fortran CLI named Forty using `fpm` unless repository conditions strongly justify another Fortran-native build path.

The initial Forty may be narrow, but it must establish the operational pattern.

Minimum Phase 0 responsibilities:

- verify the Fortran compiler and `fpm`,
- report project status,
- build the project,
- run tests,
- measure executable heresy,
- initialize Git when authorized,
- check the GitHub CLI,
- drive GitHub authentication status or login,
- create/connect the GitHub repository when explicitly authorized,
- verify the remote,
- and print a suitably solemn success message.

The one-time bootstrap may use `fpm run forty -- <command>` or a direct compiler command. After Forty owns an operation, do not bypass it with a new convenience script.

## Rule 4: GitHub meets the Cathedral through Forty

Do not manually perform the normal sequence of `git init`, `gh auth login`, `gh repo create`, remote setup, first push, and verification once Forty can drive it.

Forty should orchestrate official `git` and `gh` commands through Fortran's process execution facilities.

Forty must:

- never request, read, echo, or store access tokens,
- defer secure authentication UI to the GitHub CLI,
- ask for explicit confirmation before repository creation or push,
- verify each external command's exit status,
- fail clearly and safely,
- and show the exact external actions it intends to perform.

A reasonable command family is:

```text
forty status
forty build
forty test
forty confess
forty github status
forty github connect
forty github verify
forty serve
forty clean
```

The exact interface may improve, but the ownership principle may not.

## Rule 5: Start with the smallest real cathedral

After Forty exists, Phase 1 should prove the web premise with a complete vertical slice:

- a Fortran generator,
- a content source,
- generated HTML,
- real CSS,
- at least two pages,
- a visible architecture/confessional page,
- build and test commands driven by Forty,
- and a local run path.

Do not begin with a giant framework or broad content inventory.

## Rule 6: Prefer static generation before server complexity

Unless the initial product requires live server behavior, begin with a Fortran static-site generator.

Static generation offers the cleanest proof that Fortran owns:

- routing,
- templates,
- content assembly,
- navigation,
- page metadata,
- output generation,
- sitemap generation,
- and architecture reporting.

A Fortran server may come later when justified by an interactive feature or because it would be extremely funny and still supportable.

## Rule 7: Heresy must be measurable

Maintain `HERESY_LEDGER.md` and have Forty generate or verify measurable data where practical:

- file,
- language,
- line count,
- purpose,
- why it is unavoidable,
- whether it can later be removed.

The site should eventually display this publicly.

## Rule 8: Purity is aggressive, not dishonest

Do not write insecure cryptography, mishandle credentials, or fake platform support merely to claim 100% purity.

But do not confuse “unusual” with “unreasonable.” A bounded Fortran parser, generator, checker, or orchestrator is exactly the point even when a mature convenience tool exists elsewhere.

Use external platform tools at genuine boundaries. Let Forty drive them.

## Rule 9: Every dependency earns its place

Before adding a dependency, state:

- what problem it solves,
- why current Fortran code cannot or should not solve it,
- how it affects portability,
- whether it introduces executable non-Fortran code,
- how it will be built and tested,
- and whether Forty can wrap or inspect it.

Prefer Fortran-native dependencies and `fpm` packages when suitable.

## Rule 10: Keep deployment boring, but let Forty prepare it

The implementation may be strange. The hosting target should be dependable.

Prefer generated static files deployable to ordinary static hosting. Forty should build, validate, and prepare the deployable output and may generate declarative deployment files.

## Rule 11: Accessibility is part of the joke landing

The site must support:

- semantic HTML,
- keyboard navigation,
- readable contrast,
- reduced motion,
- responsive layouts,
- meaningful headings,
- sensible link text.

A cathedral people cannot enter is merely a facade.

## Rule 12: Test the premise

Tests should verify things such as:

- expected pages are generated,
- navigation is valid,
- content is inserted correctly,
- escaping works,
- malformed content fails clearly,
- generated artifacts are deterministic where expected,
- no unexpected executable code appears outside `heresy/`,
- the heresy ledger is accurate,
- Forty reports command failures honestly,
- GitHub connection steps are dry-runnable or safely testable,
- and the architecture page matches reality.

## Rule 13: No secret conventional stack

Forbidden evasions include:

- a Node or Python generator called by a Fortran wrapper,
- a normal web framework that receives precomputed Fortran data,
- shell scripts that quietly become the real build system,
- JavaScript that owns routing or content while Fortran emits a loading screen,
- and “temporary” scaffolding that never leaves.

## Rule 14: Stop at phase boundaries

After an approved phase:

- compile,
- test,
- inspect output,
- report,
- update the heresy ledger,
- state which operations Forty now owns,
- stop.

Do not convert a successful first page into an unsolicited content platform.
