EMACS       ?= emacs
PACKAGE_DIR := $(CURDIR)/.deps
STAMP       := $(PACKAGE_DIR)/.installed

# Optional local checkout for fzf-native. The multibyte tests
# `skip-unless' this is present, so it is purely a developer
# convenience - CI leaves it unset and those tests skip cleanly.
# Autodetect across common package manager layouts; override with
# FZF_NATIVE_LOCAL=/path/to/checkout if yours lives elsewhere.
# Prefer a checkout under `<elpa>/<emacs-major>/' when that layout is
# in use (e.g. `elpa/31/fzf-native') so the dynamic module matches the
# running Emacs's ABI.
EMACS_MAJOR := $(shell $(EMACS) -Q --batch --eval '(princ emacs-major-version)' 2>/dev/null)
FZF_ROOTS := \
  $(HOME)/.emacs.d/elpa \
  $(HOME)/.config/emacs/elpa \
  $(HOME)/.emacs.d/straight/build \
  $(HOME)/.emacs.d/elpaca/builds
FZF_NATIVE_LOCAL ?= $(firstword \
  $(foreach r,$(FZF_ROOTS), \
    $(wildcard $(r)/$(EMACS_MAJOR)/fzf-native) \
    $(wildcard $(r)/$(EMACS_MAJOR)/fzf-native-*) \
    $(wildcard $(r)/fzf-native) \
    $(wildcard $(r)/fzf-native-*) \
    $(wildcard $(r)/*/fzf-native) \
    $(wildcard $(r)/*/fzf-native-*)))
FZF_LOAD := $(if $(wildcard $(FZF_NATIVE_LOCAL)),-L $(FZF_NATIVE_LOCAL),)

PACKAGE_INIT := (progn \
  (require 'package) \
  (setq package-user-dir \"$(PACKAGE_DIR)\") \
  (setq package-archives '((\"gnu\"   . \"https://elpa.gnu.org/packages/\") \
                           (\"melpa\" . \"https://melpa.org/packages/\"))) \
  (package-initialize))

.PHONY: install compile test lint clean

$(STAMP): Makefile
	$(EMACS) --batch \
	  --eval "$(PACKAGE_INIT)" \
	  --eval "(package-refresh-contents)" \
	  --eval "(dolist (p '(flx compat package-lint)) (unless (package-installed-p p) (package-install p)))"
	@mkdir -p $(PACKAGE_DIR)
	@touch $@

install: $(STAMP)

compile: install
	$(EMACS) --batch \
	  --eval "$(PACKAGE_INIT)" \
	  -L . $(FZF_LOAD) \
	  -f batch-byte-compile fussy.el fussy-test.el

test: install
	$(EMACS) --batch \
	  --eval "$(PACKAGE_INIT)" \
	  -L . $(FZF_LOAD) \
	  -l ert -l fussy-test.el \
	  -f ert-run-tests-batch-and-exit

lint: install
	$(EMACS) --batch \
	  --eval "$(PACKAGE_INIT)" \
	  -L . $(FZF_LOAD) \
	  -l package-lint \
	  --eval "(advice-add 'package-lint--check-eval-after-load :around #'ignore)" \
	  --eval "(advice-add 'package-lint--check-version-regexp-list :around #'ignore)" \
	  -f package-lint-batch-and-exit fussy.el

clean:
	rm -f *.elc benchmark/*.elc
	rm -rf $(PACKAGE_DIR)
