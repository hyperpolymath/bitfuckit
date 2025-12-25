; SPDX-License-Identifier: AGPL-3.0-or-later
; META.scm - Meta-level information for bitfuckit

(meta
  (version "1.0.0")
  (schema-version "1.0")
  (media-type "application/meta+scheme")

  (architecture-decisions
    (adr-001
      (title "Use Ada for CLI implementation")
      (status accepted)
      (date "2025-12-25")
      (context "Need a reliable CLI tool for Bitbucket. Options: Rust, Go, Python, Ada.")
      (decision "Use Ada/SPARK for implementation due to:
        - Strong type safety and compile-time guarantees
        - SPARK subset enables formal verification
        - Native compilation without runtime dependencies
        - Excellent for safety-critical and reliable software
        - Aligns with RSR language policy")
      (consequences
        (positive "High reliability, formal verification possible, no runtime deps")
        (negative "Smaller ecosystem than Rust, fewer available libraries")))

    (adr-002
      (title "Use curl subprocess for HTTP")
      (status accepted)
      (date "2025-12-25")
      (context "Need to make HTTP requests to Bitbucket API. Options: AWS.Client, libcurl bindings, curl subprocess.")
      (decision "Use curl subprocess via GNAT.OS_Lib.Spawn")
      (consequences
        (positive "Simple, portable, no additional dependencies")
        (negative "Subprocess overhead, less control over HTTP details")))

    (adr-003
      (title "SPARK Mode for TUI specification")
      (status accepted)
      (date "2025-12-25")
      (context "TUI module has clear interface that could benefit from formal verification.")
      (decision "Enable SPARK_Mode for tui.ads specification, disable for body due to I/O")
      (consequences
        (positive "Interface contracts formally verified")
        (negative "Body cannot be verified due to I/O operations"))))

  (development-practices
    (code-style "Ada 2012 style guide, 3-space indentation")
    (security "Credentials stored with user-only permissions, no secrets in code")
    (testing "Manual testing for now, automated tests planned")
    (versioning "Semantic versioning (semver)")
    (documentation "AsciiDoc for docs, Ada comments for code")
    (branching "main is stable, feature branches for development"))

  (design-rationale
    (why-ada "Ada provides the reliability and safety guarantees needed for a tool that manages git forges. SPARK subset allows formal verification of critical interfaces.")
    (why-tui "Interactive mode improves UX for complex operations like browsing repos")
    (why-curl "Minimizes dependencies while providing full HTTP capability")
    (why-app-password "Bitbucket requires app passwords for API access, more secure than storing account password")))
