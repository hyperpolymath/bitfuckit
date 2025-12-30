;; SPDX-License-Identifier: AGPL-3.0-or-later
;; STATE.scm - Current project state for bitfuckit

(define project-state
  `((metadata
      ((version . "0.1.0")
       (schema-version . "1")
       (created . "2025-12-25T00:00:00+00:00")
       (updated . "2025-12-30T00:00:00+00:00")
       (project . "bitfuckit")
       (repo . "bitfuckit")
       (tagline . "Bitbucket CLI tool - the one Atlassian didn't make")))

    (project-context
      ((name . "bitfuckit")
       (purpose . "Command-line interface for Bitbucket Cloud, filling the gap left by Atlassian")
       (tech-stack . ("Ada 2022" "SPARK" "GPRbuild" "GraphQL"))
       (primary-language . "ada")))

    (current-position
      ((phase . "v0.1.0-released")
       (overall-completion . 25)
       (components
         ((authentication
            ((status . "complete")
             (completion . 100)
             (features . ("login" "status" "app-password-storage"))))
          (repository-ops
            ((status . "complete")
             (completion . 100)
             (features . ("create" "list" "delete" "exists"))))
          (pull-requests
            ((status . "partial")
             (completion . 30)
             (features . ("list"))
             (missing . ("create" "merge" "decline" "approve"))))
          (mirroring
            ((status . "complete")
             (completion . 100)
             (features . ("github-to-bitbucket"))))
          (tui
            ((status . "complete")
             (completion . 100)
             (features . ("vim-navigation" "color-interface" "menu-selection"))))
          (graphql-api
            ((status . "partial")
             (completion . 40)
             (features . ("schema" "basic-server"))
             (missing . ("full-resolvers" "subscriptions"))))))
       (working-features
         . ("auth login" "auth status" "repo create" "repo list"
            "repo delete" "repo exists" "pr list" "mirror" "tui"))))

    (route-to-mvp
      ((milestones
        ((v0.1.0
           ((name . "Foundation")
            (status . "released")
            (date . "2025-12-30")
            (items . ("Authentication" "Repository CRUD" "PR listing"
                      "GitHub mirror" "Interactive TUI" "SPARK verification"))))
         (v0.2.0
           ((name . "Pull Request Workflows")
            (status . "planned")
            (target . "Q1 2025")
            (items . ("PR create" "PR merge" "PR decline" "PR comments"
                      "PR diff" "PR approve"))))
         (v0.3.0
           ((name . "Issue Tracking")
            (status . "planned")
            (target . "Q2 2025")
            (items . ("Issue list" "Issue create" "Issue update" "Issue comments"))))
         (v0.4.0
           ((name . "Pipeline Integration")
            (status . "planned")
            (target . "Q2 2025")
            (items . ("Pipeline status" "Pipeline trigger" "Pipeline logs"))))
         (v0.5.0
           ((name . "Team & Access")
            (status . "planned")
            (target . "Q3 2025")
            (items . ("Team list" "Branch permissions" "Deploy keys" "Webhooks"))))
         (v1.0.0
           ((name . "Feature Parity")
            (status . "planned")
            (target . "Q4 2025")
            (items . ("Windows support" "Full test suite" "All CRUD ops"))))))))

    (blockers-and-issues
      ((critical . ())
       (high
         . (((id . "SEC-001")
             (description . "Shell injection risk in API calls")
             (mitigation . "Input sanitization needed"))
            ((id . "SEC-002")
             (description . "Plain text credential storage")
             (mitigation . "Integrate with libsecret/keyring"))))
       (medium
         . (((id . "PERF-001")
             (description . "Process-spawned curl for HTTP")
             (mitigation . "Consider native HTTP library"))))
       (low . ())))

    (critical-next-actions
      ((immediate
         . ("Create v0.2.0 branch for PR workflows"
            "Document security improvements in SECURITY.md"))
       (this-week
         . ("Implement pr create command"
            "Add input sanitization to API calls"))
       (this-month
         . ("Complete v0.2.0 PR workflows"
            "Add AUnit test framework"))))

    (session-history
      ((2025-12-30
         ((accomplishments
            . ("Released v0.1.0"
               "Updated README with unofficial disclaimer"
               "Enhanced justfile with combinatoric recipes"
               "Fixed ROADMAP.adoc"
               "Updated config.ncl with documented cookbook"
               "Cleaned up cruft files"))
          (decisions
            . ("AGPL license to ensure community contributions if Atlassian uses this"
               "SPARK verification for TUI to demonstrate reliability"))))))))

;; Helper functions
(define (get-completion-percentage state)
  (cadr (assoc 'overall-completion (cadr (assoc 'current-position state)))))

(define (get-blockers state priority)
  (cadr (assoc priority (cadr (assoc 'blockers-and-issues state)))))

(define (get-milestone state version)
  (cadr (assoc version (cadr (assoc 'milestones (cadr (assoc 'route-to-mvp state)))))))
