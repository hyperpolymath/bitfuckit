;; SPDX-License-Identifier: MPL-2.0
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
;;
;; Guix package definition for bitfuckit
;;
;; Usage:
;;   guix shell -D -f guix.scm    # Enter development shell
;;   guix build -f guix.scm       # Build package
;;
;; TODO: wire real Zig build/check phases and inputs below — the phases and
;; native-inputs/inputs are still template stubs; `guix build -f guix.scm`
;; currently only copies README.adoc, it does not build the Zig FFI layer.
;; See: https://guix.gnu.org/manual/en/html_node/Defining-Packages.html

(use-modules (guix packages)
             (guix gexp)
             (guix git-download)
             (guix build-system gnu)
             (guix licenses)
             (gnu packages base))

(package
  (name "bitfuckit")
  (version "0.1.0")
  (source (local-file "." "source"
                       #:recursive? #t
                       #:select? (lambda (file stat)
                                   (not (string-contains file ".git")))))
  (build-system gnu-build-system)
  (arguments
   '(#:phases
     (modify-phases %standard-phases
       ;; TODO: Customize build phases for your project
       ;; Examples for common stacks:
       ;;
       ;; Rust:
       ;;   (replace 'build (lambda _ (invoke "cargo" "build" "--release")))
       ;;   (replace 'check (lambda _ (invoke "cargo" "test")))
       ;;
       ;; Elixir:
       ;;   (replace 'build (lambda _ (invoke "mix" "compile")))
       ;;   (replace 'check (lambda _ (invoke "mix" "test")))
       ;;
       ;; Zig:
       ;;   (replace 'build (lambda _ (invoke "zig" "build")))
       ;;   (replace 'check (lambda _ (invoke "zig" "build" "test")))
       (delete 'configure)
       (delete 'build)
       (delete 'check)
       (replace 'install
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (mkdir-p (string-append out "/share/doc"))
             (copy-file "README.adoc"
                        (string-append out "/share/doc/README.adoc"))))))))
  (native-inputs
   (list
    ;; TODO: Add build-time dependencies
    ;; Examples:
    ;;   rust (gnu packages rust)
    ;;   elixir (gnu packages elixir)
    ;;   zig (gnu packages zig)
    ))
  (inputs
   (list
    ;; TODO: Add runtime dependencies
    ))
  (home-page "https://github.com/hyperpolymath/bitfuckit")
  (synopsis "Ada/SPARK repository auditor")
  (description "bitfuckit audits Ada/SPARK repositories for health, compliance,
and security posture.  It is registered in the reposystem tool ecosystem as a
utility-role tool with a single declared action, @code{audit}.  See
README.adoc and .machine_readable/6a2/STATE.a2ml for current implementation
status.")
  (license (list
            ;; MPL-2.0 extends MPL-2.0
            mpl2.0)))
