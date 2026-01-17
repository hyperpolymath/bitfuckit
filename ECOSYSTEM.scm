; SPDX-License-Identifier: PMPL-1.0
; ECOSYSTEM.scm - Project's place in the ecosystem

(ecosystem
  (version "1.0.0")
  (name "bitfuckit")
  (type "cli-tool")
  (purpose "Bitbucket CLI tool - the missing official CLI")
  (media-type "application/vnd.ecosystem+scm")

  (position-in-ecosystem
    (category "developer-tools/vcs-cli")
    (fills-gap "Atlassian never provided a CLI for Bitbucket")
    (complements ("gh" "glab" "tea" "hut" "rad"))
    (part-of "hyperpolymath forge tooling ecosystem"))

  (related-projects
    (sibling-standard
      (gh "GitHub CLI - the inspiration and reference")
      (glab "GitLab CLI - similar role for GitLab")
      (tea "Gitea/Codeberg CLI - Forgejo ecosystem"))

    (sibling-project
      (forge-mirror "Universal forge mirroring script using bitfuckit")
      (hut "SourceHut CLI")
      (rad "Radicle CLI"))

    (potential-consumer
      (gitvisor "Git supervisor tool that could orchestrate forge CLIs")
      (robot-repo-cleaner "Repo maintenance that could use bitfuckit for Bitbucket ops"))

    (inspiration
      (gh "Command structure and UX patterns")
      (cobra "Go CLI framework patterns adapted to Ada")))

  (what-this-is
    "A command-line interface for Bitbucket Cloud"
    "Written in Ada/SPARK for reliability"
    "Includes interactive TUI mode"
    "Part of multi-forge workflow tooling")

  (what-this-is-not
    "Not an official Atlassian product"
    "Not a Bitbucket Server/Data Center tool (Cloud only)"
    "Not a full git replacement (complements git)"
    "Not a CI/CD tool (manages repos, not pipelines yet)"))
