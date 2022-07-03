(package "fussy"
         "0.1"
         "Fuzzy completion style using `flx'")

(package-file "fussy.el")

(source "gnu")
(source "melpa")

(depends-on "emacs" "27.1")
(depends-on "flx")

(development
 (depends-on "f")
 (depends-on "ert-runner")
 (depends-on "package-lint")
 (depends-on "orderless")
 (depends-on "fzf-native" :fetcher 'github :repo "dangduc/fzf-native" :files '(:defaults "bin")))

 (setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
