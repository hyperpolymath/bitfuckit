; SPDX-License-Identifier: MPL-2.0-or-later
; AGENTIC.scm - AI agent interaction patterns

(agentic
  (version "1.0.0")
  (schema-version "1.0")

  (agent-capabilities
    (can-do
      "Build project with gprbuild"
      "Run tests if present"
      "Check SPARK verification status"
      "Update documentation"
      "Add new CLI commands"
      "Improve error handling")

    (cannot-do
      "Change language from Ada (policy violation)"
      "Add npm/Python/Go dependencies"
      "Modify user's Bitbucket credentials"
      "Push to remote without permission"))

  (interaction-patterns
    (build
      (command "gprbuild -P bitfuckit.gpr")
      (output-dir "bin/")
      (binary "bitfuckit"))

    (clean
      (command "gprclean -P bitfuckit.gpr"))

    (verify-spark
      (command "gnatprove -P bitfuckit.gpr --mode=check")
      (note "Verifies SPARK contracts in tui.ads"))

    (test
      (command "bin/bitfuckit --help")
      (note "Basic smoke test")))

  (code-patterns
    (add-command
      (location "src/bitfuckit.adb")
      (pattern "Add case branch in main dispatch")
      (example "
        elsif Cmd = \"newcmd\" then
           Do_New_Command;"))

    (add-api-function
      (location "src/bitbucket_api.adb")
      (pattern "Add function with API_Result return type")
      (uses-curl #t))

    (add-tui-menu-item
      (location "src/tui.adb")
      (pattern "Extend Main_Menu array in Run_TUI")))

  (safety-constraints
    (never-modify
      ".claude/CLAUDE.md"
      "LICENSE"
      "User credentials")
    (preserve
      "SPDX headers on all files"
      "Ada/SPARK language choice"
      "GPRbuild as build system")))
