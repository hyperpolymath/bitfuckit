; SPDX-License-Identifier: PMPL-1.0
; NEUROSYM.scm - Neurosymbolic reasoning patterns

(neurosym
  (version "1.0.0")
  (schema-version "1.0")

  (domain-model
    (entities
      (bitbucket-workspace
        (attributes username app-password repos)
        (relations (owns repo)))

      (repository
        (attributes name slug is-private description)
        (relations (belongs-to workspace) (has-many branches)))

      (credentials
        (attributes username app-password workspace)
        (storage "~/.config/bitfuckit/config")
        (security user-only-permissions)))

    (operations
      (authenticate
        (precondition "Has app password")
        (postcondition "Credentials stored locally")
        (idempotent #t))

      (create-repo
        (precondition "Authenticated and repo does not exist")
        (postcondition "Repo exists on Bitbucket")
        (idempotent #f))

      (delete-repo
        (precondition "Authenticated and repo exists")
        (postcondition "Repo no longer exists")
        (requires-confirmation #t)
        (idempotent #t))

      (mirror
        (precondition "In git repo, authenticated, source has commits")
        (postcondition "Bitbucket repo contains all source branches")
        (idempotent #t))))

  (reasoning-patterns
    (error-diagnosis
      (pattern "API returns 401")
      (inference "Credentials invalid or expired")
      (action "Re-run auth login"))

    (pattern "API returns 404 on repo operation")
      (inference "Repo does not exist or wrong workspace")
      (action "Check workspace and repo name"))

    (pattern "Push fails with permission denied")
      (inference "SSH key not in Bitbucket or app password lacks repo:write")
      (action "Add SSH key or update app password permissions"))

  (invariants
    (credentials-never-logged
      "App password must never appear in logs or error messages")

    (confirmation-before-delete
      "Delete operations require explicit user confirmation")

    (atomic-config-writes
      "Config file writes are atomic to prevent corruption")))
