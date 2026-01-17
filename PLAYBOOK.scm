; SPDX-License-Identifier: PMPL-1.0
; PLAYBOOK.scm - Operational playbooks

(playbook
  (version "1.0.0")
  (schema-version "1.0")

  (setup
    (name "Initial Setup")
    (steps
      (step "Install GNAT compiler"
        (fedora "sudo dnf install gcc-gnat")
        (debian "sudo apt install gnat"))
      (step "Install GPRbuild"
        (fedora "sudo dnf install gprbuild")
        (debian "sudo apt install gprbuild"))
      (step "Clone and build"
        (commands
          "git clone https://github.com/hyperpolymath/bitfuckit.git"
          "cd bitfuckit"
          "gprbuild -P bitfuckit.gpr"))
      (step "Install binary"
        (command "cp bin/bitfuckit ~/.local/bin/"))))

  (authenticate
    (name "Bitbucket Authentication")
    (prerequisites
      "Bitbucket account"
      "App password with repo permissions")
    (steps
      (step "Create app password"
        (url "https://bitbucket.org/account/settings/app-passwords/")
        (permissions "repository:read" "repository:write" "repository:delete"))
      (step "Run login"
        (command "bitfuckit auth login"))
      (step "Verify"
        (command "bitfuckit auth status"))))

  (mirror-workflow
    (name "Mirror from GitHub to Bitbucket")
    (prerequisites
      "Authenticated with bitfuckit"
      "Inside a git repository"
      "SSH key added to Bitbucket")
    (steps
      (step "Ensure in repo directory"
        (command "git status"))
      (step "Mirror to Bitbucket"
        (command "bitfuckit mirror <repo-name>"))
      (step "Verify on Bitbucket"
        (url "https://bitbucket.org/<workspace>/<repo>"))))

  (troubleshooting
    (issue "401 Unauthorized"
      (cause "Invalid or expired app password")
      (fix "bitfuckit auth login"))

    (issue "404 Not Found"
      (cause "Wrong workspace or repo name")
      (fix "Check workspace with: bitfuckit auth status"))

    (issue "Permission denied on push"
      (cause "SSH key not registered or app password lacks write permission")
      (fix "Add SSH key at bitbucket.org/account/settings/ssh-keys/"))

    (issue "gprbuild not found"
      (cause "GPRbuild not installed")
      (fix "sudo dnf install gprbuild"))

    (issue "TUI garbled display"
      (cause "Terminal doesn't support ANSI or Unicode")
      (fix "Use a modern terminal emulator (kitty, alacritty, gnome-terminal)")))

  (release
    (name "Release Checklist")
    (steps
      (step "Update version in STATE.scm")
      (step "Update CHANGELOG if present")
      (step "Build and test"
        (commands
          "gprbuild -P bitfuckit.gpr"
          "bin/bitfuckit --help"))
      (step "Tag release"
        (command "git tag -s v0.1.0 -m 'Release v0.1.0'"))
      (step "Push with tags"
        (command "git push --tags origin main"))
      (step "Create GitHub release"
        (command "gh release create v0.1.0 --generate-notes")))))
