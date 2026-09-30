<!--
SPDX-License-Identifier: MPL-2.0-or-later
-->

# Contributing to bitfuckit

Thank you for your interest in contributing to bitfuckit - the
community-built Bitbucket CLI that Atlassian never made!

## Language Policy

bitfuckit follows the hyperpolymath [Rhodium
Standard](https://github.com/hyperpolymath/rhodium-standard-repositories):

| Language         | Use Case                 | Notes                           |
|------------------|--------------------------|---------------------------------|
| **Ada 2012**     | Primary application code | SPARK verified where possible   |
| **Nickel**       | Configuration            | For complex config (config.ncl) |
| **Bash/POSIX**   | Scripts, automation      | Keep minimal                    |
| **Guile Scheme** | State/meta files         | STATE.scm, META.scm, etc.       |

**BANNED**: TypeScript, Node.js, npm, Go, Python (except SaltStack)

## Getting Started

1.  Fork the repository

2.  Clone your fork:

    ``` bash
    git clone https://github.com/YOUR_USERNAME/bitfuckit.git
    cd bitfuckit
    ```

<!-- -->

1.  Install dependencies:

    ``` bash
    # Fedora
    sudo dnf install gcc-gnat gprbuild

    # Debian/Ubuntu
    sudo apt install gnat gprbuild
    ```

<!-- -->

1.  Build:

    ``` bash
    gprbuild -P bitfuckit.gpr
    ```

## Code Style

- Ada 2012 style guide

- 3-space indentation

- SPDX license header on all source files

- Use SPARK Mode where possible for specification files

## Submitting Changes

1.  Create a feature branch:

    ``` bash
    git checkout -b feature/my-feature
    ```

<!-- -->

1.  Make your changes

2.  Ensure the build succeeds:

    ``` bash
    gprbuild -P bitfuckit.gpr
    bin/bitfuckit --help
    ```

<!-- -->

1.  Commit with a descriptive message

2.  Push and create a pull request

## Adding New Commands

1.  Add the command handling in `src/bitfuckit.adb`

2.  If API calls needed, add to `src/bitbucket_api.adb`

3.  Update the help text in `Print_Usage`

4.  Add shell completions in `completions/`

5.  Update the man page in `doc/bitfuckit.1`

## Questions?

Open an issue at <https://github.com/hyperpolymath/bitfuckit/issues>

## Signed commits

Every commit that reaches the default branch must be signed; a ruleset refuses
unsigned pushes. Estate policy:
[SIGNING-POLICY](https://github.com/hyperpolymath/standards/blob/main/docs/SIGNING-POLICY.adoc).

- **People and interactive agents** sign with an SSH key registered on GitHub
  as a *signing* key (`gpg.format=ssh`, `user.signingkey=<key>.pub`,
  `commit.gpgsign=true`). The committer email must be verified on that account.
- **Apps, bots and workflows** never `git push` local commits. They write
  through the API (`createCommitOnBranch` or the estate `signed-push` action)
  so that GitHub signs each commit.
- Merge PRs with **squash**. The ruleset checks every commit on the PR branch,
  not just the result, so one unsigned commit blocks the merge. Re-create such a
  branch with signed commits (`git cherry-pick -S`) and open a new PR.
  Rebase-merge replays commits unsigned and is disabled.
