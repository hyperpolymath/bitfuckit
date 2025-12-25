; SPDX-License-Identifier: AGPL-3.0-or-later
; STATE.scm - Current project state

(state
  (metadata
    (version "0.1.0-alpha")
    (schema-version "1.0")
    (created "2025-12-25")
    (updated "2025-12-25")
    (project "bitfuckit")
    (repo "https://github.com/hyperpolymath/bitfuckit"))

  (project-context
    (name "bitfuckit")
    (tagline "Bitbucket CLI tool in Ada - they didn't make it, so I did")
    (tech-stack
      (language "Ada 2012")
      (verification "SPARK 2014")
      (build "GPRbuild")
      (http "curl subprocess")))

  (current-position
    (phase "alpha")
    (overall-completion 40)
    (components
      (cli-core (status complete) (completion 100))
      (auth (status complete) (completion 100))
      (repo-crud (status complete) (completion 100))
      (mirror (status complete) (completion 100))
      (tui (status complete) (completion 100))
      (pr-management (status planned) (completion 0))
      (pipeline-status (status planned) (completion 0))
      (packaging (status in-progress) (completion 10)))
    (working-features
      "auth login/status"
      "repo create/list/delete/exists"
      "mirror from GitHub"
      "interactive TUI"))

  (route-to-mvp
    (milestone-1
      (name "Core CLI")
      (status complete)
      (items
        (item "Authentication" complete)
        (item "Repository CRUD" complete)
        (item "Mirror command" complete)))

    (milestone-2
      (name "User Experience")
      (status complete)
      (items
        (item "Interactive TUI" complete)
        (item "Vim-style navigation" complete)
        (item "Color output" complete)))

    (milestone-3
      (name "Distribution")
      (status in-progress)
      (items
        (item "Comprehensive README" complete)
        (item "SCM files" complete)
        (item "Man page" pending)
        (item "Shell completions" pending)
        (item "Package builds" pending)))

    (milestone-4
      (name "Extended Features")
      (status planned)
      (items
        (item "Pull request management" pending)
        (item "Pipeline status" pending)
        (item "Issue tracking" pending))))

  (blockers-and-issues
    (critical)
    (high
      (issue "Needs gprbuild to compile - not commonly installed"))
    (medium
      (issue "No automated tests yet")
      (issue "Error handling could be more robust"))
    (low
      (issue "TUI menu actions are stubs")))

  (critical-next-actions
    (immediate
      "Add justfile for build automation"
      "Add man page"
      "Create shell completions")
    (this-week
      "Tag v0.1.0-alpha release"
      "Set up GitHub release workflow")
    (this-month
      "Package for Fedora COPR"
      "Implement PR management commands"))

  (session-history
    (session-001
      (date "2025-12-25")
      (accomplishments
        "Created bitfuckit repo"
        "Implemented full CLI in Ada"
        "Added SPARK TUI interface"
        "Created comprehensive README"
        "Added SCM files"))))
