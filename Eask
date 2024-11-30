(package "fussy"
         "1.0"
         "Fuzzy completion style using `flx'")

(website-url "https://github.com/jojojames/fussy")
(keywords "matching")

(package-file "fussy.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(depends-on "emacs" "28.1")
(depends-on "flx")

(development
 (depends-on "f")
 (depends-on "ert-runner")
 (depends-on "package-lint")
 (depends-on "orderless"))
 
(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

(add-hook 'eask-before-lint/package-hook
           (lambda (&rest _)
             (advice-add 'package-lint--check-eval-after-load :around 'ignore)
             (advice-add 'package-lint--check-version-regexp-list :around 'ignore)))
