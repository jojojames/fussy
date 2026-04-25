;; -*- mode: eask; lexical-binding: t -*-
(package "fussy"
  "1.0"
  "Fuzzy completion style using `flx' and `fzf'")
(package-file "fussy.el")
;; Sources
(source "melpa")
(source "gnu")
;; Runtime dependencies
(depends-on "emacs" "29.1")
(depends-on "flx")
(depends-on "compat")

;;; Platform detection.
;;;
;;; `fzf-native' ships pre-built dynamic modules for Darwin, Linux,
;;; FreeBSD, and Windows. On any other host (Haiku, Hurd, Android, ...)
;;; there is no usable binary, so we don't even try to wire it up - the
;;; multibyte tests in `fussy-test.el' detect the missing module and
;;; `skip-unless' themselves out.
(defconst MAC-P     (eq system-type 'darwin))
(defconst LINUX-P   (eq system-type 'gnu/linux))
(defconst WINDOWS-P (memq system-type '(cygwin windows-nt ms-dos)))
(defconst BSD-P     (eq system-type 'berkeley-unix))
(defconst FZF-NATIVE-SUPPORTED-P (or MAC-P LINUX-P WINDOWS-P BSD-P))

;;; Wire up `fzf-native' from a local checkout.
;;;
;;; Why `load-paths' and not `depends-on ... :file': eask's `:file'
;;; install path copies only `.el' files into the sandbox - it ignores
;;; the `(files ...)' directive in the package's own Eask file, so
;;; non-elisp resources get dropped. fzf-native bundles its prebuilt
;;; dynamic modules under `bin/<platform>/'; without those, calling
;;; `fzf-native-load-dyn' signals "module not found".
;;;
;;; `load-paths' adds the source directory directly to `load-path'.
;;; `(require 'fzf-native)' then loads `fzf-native.el' in place, and
;;; `fzf-native-load-dyn' computes the bin path from `load-file-name'
;;; which points at the real checkout where `bin/' is intact.
;;;
;;; This is a developer-machine convenience, not a CI dependency. If
;;; the directory below doesn't exist on disk, `load-paths' adds a
;;; non-existent entry (harmless) and the multibyte tests skip
;;; themselves cleanly via `skip-unless'.
;;;
;;; Resolution order:
;;;   1. $FZF_NATIVE_LOCAL          - explicit override path
;;;   2. ~/.emacs.d/elpa/fzf-native - the standard local checkout
;;; (We can't use `user-emacs-directory' here because eask rebinds it
;;; to the workspace's .eask/ sandbox while the Eask file is loading.)
(when FZF-NATIVE-SUPPORTED-P
  (let* ((home (or (getenv "HOME") "~"))
         (default (expand-file-name ".emacs.d/elpa/fzf-native" home))
         (local (or (getenv "FZF_NATIVE_LOCAL") default)))
    (if (file-directory-p local)
        (load-paths local)
      (eask-warn
       "fzf-native local checkout not found at %s; multibyte tests will skip"
       local))))

;; Development dependencies
(development
  (depends-on "f")
  (depends-on "ert-runner")
  (depends-on "package-lint")
  (depends-on "orderless"))

;; Lint workaround (scoped)
(add-hook 'eask-before-lint/package-hook
  (lambda (&rest _)
    (advice-add #'package-lint--check-eval-after-load :around #'ignore)
    (advice-add #'package-lint--check-version-regexp-list :around #'ignore)))
