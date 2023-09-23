;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

;;; Commentary:
;;
;; GNU Guix development package.  To build and install, clone this repository,
;; switch directory to here and run:
;;
;;   guix package --install-from-file=guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix shell --container -D -f guix.scm
;;
;; Replace --container by --pure if you still want ASDF to see external
;; libraries in ~/common-lisp, etc.
;;
;;; Code:

(use-modules (guix packages)
             ((guix licenses) #:prefix license:)
             (guix gexp)
             (guix git-download)
             (guix build-system asdf)
             (gnu packages)
             (gnu packages lisp)
             (gnu packages lisp-xyz))

(define-public sbcl-trivial-toplevel-commands
  (package
   (name "sbcl-trivial-toplevel-commands")
   (version "0.0.1")
   (source
    (local-file (dirname (current-filename)) #:recursive? #t)
    ;;;; Or this, in case of contributing to Guix.
    ;; (origin
    ;;   (method git-fetch)
    ;;   (uri (git-reference
    ;;         (url "https://github.com/aartaka/trivial-toplevel-commands")
    ;;         (commit version)))
    ;;   (file-name (git-file-name "cl-trivial-toplevel-commands" version))
    ;;   (sha256
    ;;    (base32
    ;;     "SPECIFY-HASH")))
    )
   (build-system asdf-build-system/sbcl)
   (synopsis "ADD A SYNOPSYS.")
   (home-page "https://github.com/aartaka/trivial-toplevel-commands")
   (description "ADD A DESCRIPTION")
   (license license:bsd-3)))

(define-public cl-trivial-toplevel-commands
  (sbcl-package->cl-source-package sbcl-trivial-toplevel-commands))

(define-public ecl-trivial-toplevel-commands
  (sbcl-package->ecl-package sbcl-trivial-toplevel-commands))

cl-trivial-toplevel-commands
