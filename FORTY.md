# Forty — The Cathedral's Fortran Operator

## Identity

**Forty** is a real command-line application written in Fortran.

Forty is the Cathedral's verger: it unlocks the doors, checks the machinery, counts the impurities, introduces the repository to GitHub, and announces whether the structure still stands.

Forty is not a mascot attached to shell scripts. Forty must contain the orchestration logic.

## Why Forty exists

Most projects use a pile of shell commands, package scripts, CI snippets, and setup notes.

Fortran Cathedral uses a Fortran program for that because this project has standards.

The existence of Forty demonstrates the founding principle:

> The dumbest reasonable task should be written in Fortran. The dumber the task, the better the joke — provided the result is safe and works.

## Initial command surface

The first implementation may be small, but it should establish commands such as:

```text
forty help
forty status
forty doctor
forty build
forty test
forty confess
forty github status
forty github connect
forty github verify
forty serve
forty clean
```

Possible later commands:

```text
forty generate
forty check-links
forty routes
forty release
forty deploy
forty open
forty bless
```

Names may evolve, but project operations should converge on Forty rather than proliferating conventional scripts.

## GitHub connection contract

`forty github connect` should drive the repository's GitHub setup.

It may invoke official external programs using Fortran process execution, but Forty owns the sequence and interpretation.

Expected flow:

1. Confirm the command is running inside the Cathedral repository.
2. Verify `git` is available.
3. Verify the GitHub CLI (`gh`) is available.
4. Check whether Git is initialized.
5. Offer to initialize it only with explicit user confirmation.
6. Check GitHub authentication with `gh auth status`.
7. When authentication is missing, explain that the official GitHub CLI will handle credentials and invoke `gh auth login` only after confirmation.
8. Collect or accept repository name, owner, description, and visibility without handling secrets.
9. Show the exact intended actions.
10. Ask for final confirmation before creating the remote repository or pushing.
11. Invoke the appropriate `gh repo create` / Git commands.
12. Verify the remote URL and default branch.
13. Verify that the first push succeeded when requested.
14. Print:

```text
CONNECTION CONSECRATED.
THE REMOTE IS CANONICAL.
```

## Security boundary

Forty must never:

- ask the user to paste a GitHub token,
- read token files,
- log credentials,
- inspect browser cookies,
- invent an authentication flow,
- or store secrets in configuration.

The GitHub CLI owns secure authentication. Forty owns the absurdly solemn orchestration.

## External command discipline

For every delegated command, Forty should:

- display what it is about to do,
- support a non-destructive status or dry-run path where practical,
- capture and check exit status,
- stop on failure,
- provide a useful error,
- avoid destructive defaults,
- and avoid shell interpolation of untrusted text.

Platform-specific behavior should be isolated behind Fortran modules rather than scattered string commands.

## Build ownership

Prefer `fpm` as the Fortran-native build entry point.

A one-time bootstrap can be:

```text
fpm run forty -- doctor
```

Afterward, documentation should teach users to think in Forty commands, even when Forty delegates compilation to `fpm`.

## Personality

Forty's output should be concise, solemn, and completely straight-faced.

Good:

```text
COMPILER FOUND.
HERESY MEASURED: 0 EXECUTABLE LINES.
REMOTE ABSENT.
AWAITING CONSECRATION.
```

Bad:

- constant jokes,
- meme spam,
- fake errors,
- obscuring actual failures,
- theatrical output so verbose that the tool becomes annoying.

Forty is funny because it takes its duties seriously.

## Definition of done for Phase 0

Phase 0 is complete when:

- Forty compiles through the chosen Fortran-native build path,
- its command parser and exit codes work,
- `doctor`, `status`, `build`, `test`, and `confess` have credible behavior,
- GitHub setup can be driven safely through Forty,
- external side effects require confirmation,
- failures are tested without touching a real remote where possible,
- and there is no parallel shell or Node setup path quietly doing the same work.
