# Versioning

This repository uses Release Please to manage package versions.

Release Please reads Conventional Commit messages on `main`, opens a release pull request, updates `DESCRIPTION` and `NEWS.md`, and creates the GitHub release after the release pull request is merged.

Use these commit prefixes:

* `fix:` for a patch release, for example `0.0.2` -> `0.0.3`.
* `feat:` for a minor release, for example `0.0.2` -> `0.1.0`.
* `feat!:` or a `BREAKING CHANGE:` footer for a major release, for example `0.0.2` -> `1.0.0`.
* `docs:`, `test:`, `chore:`, and `refactor:` are useful for maintenance commits; they do not normally create a release unless marked as breaking.

When unsure, use:

* `fix:` for corrections or small behavioral improvements.
* `feat:` for new exported functions, new user-facing options, or new workflows.
* `feat!:` only when existing user code may need to change.
