(use-modules (guix packages)
             (guix git-download)
             (guix licenses)
             (guix gexp)
             (guix utils)
             (guix build-system haskell)
             (guix build-system trivial)
             (gnu packages commencement)
             (gnu packages haskell)
             (gnu packages haskell-apps)
             (gnu packages haskell-check)
             (gnu packages build-tools))

;; Hack to access the unexported license constructor.
;;
;; Taken from: https://gitlab.com/nonguix/nonguix/-/blob/master/nonguix/licenses.scm
(define license (@@ (guix licenses) license))

(define cc-by-nc-sa4.0
  (license "CC-BY-NC-SA 4.0"
           "http://creativecommons.org/licenses/by-nc-sa/4.0"
           "Attribution-NonCommercial-ShareAlike 4.0 International"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Build pdpmake from git for POSIX compliance fixes which are not
;; contained in the latest releases and cause mach test failures.
(define pdpmake-git
  (let ((commit "d5214d453662c484378dd7254520c2fb27571057"))
    (package
      (inherit pdpmake)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/rmyorston/pdpmake.git")
                       (commit commit)))
                (file-name (git-file-name "pdpmake" commit))
                (sha256 (base32 "1vrkb7as8hbbn1w4pdmcz88gwacr7j45pdqr7dlkyy54pxd39nhg")))))))

;; Provides a symlink from /bin/cc to the C compiler for the current
;; target. Needed for portable cc(1) invocations in the golden tests.
(define cc-symlink
  (package
    (name "cc-symlink")
    (version "1.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
      `(#:modules ((guix build utils))
        #:builder
        ,#~(begin
             (use-modules (guix build utils))
             (let ((bindir (string-append #$output "/bin")))
               (mkdir-p bindir)
               (symlink (string-append #$gcc-toolchain "/bin/gcc")
                        (string-append bindir "/cc"))))))
    (inputs (list gcc-toolchain))
    (synopsis "Provides a cc(1) symlink to gcc(1)")
    (description "")
    (home-page "")
    (license cc0)))

(package
  (name "mach")
  (version "0.1.0.0")
  (source (local-file "." "git-checkout"
                      #:recursive? #t))
  (build-system haskell-build-system)
  (inputs
    (list
      ghc-9.2
      cc-symlink))
  (native-inputs
    (list
      pdpmake-git
      gcc-toolchain
      ghc-tasty
      ghc-tasty-hunit
      ghc-tasty-golden))
  (synopsis "A WiP POSIX Make implementation")
  (description "")
  (home-page "https://github.com/nmeum/mach")
  (license (list cc-by-nc-sa4.0)))
