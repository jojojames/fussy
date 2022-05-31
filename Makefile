export EMACS ?= $(shell which emacs)
CASK_DIR := $(shell cask package-directory)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: compile
compile: cask
	! (cask eval "(let ((byte-compile-error-on-warn t)) \
	                 (cask-cli/build))" 2>&1 \
	   | egrep -a "(Warning|Error):") ; \
	  (ret=$$? ; cask clean-elc && exit $$ret)

.PHONY: test
test: compile
	cask emacs --batch -L ./test -l fussy-test -f ert-run-tests-batch

.PHONY: lint
lint: compile
	cask exec emacs -Q -batch							\
	--eval "(require 'package)"							\
	--eval "(push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives)"	\
	--eval "(package-initialize)"							\
	--eval "(package-refresh-contents)"						\
	-l package-lint.el								\
	--eval "(advice-add 'package-lint--check-eval-after-load :around 'ignore)"	\
	--eval "(advice-add 'package-lint--check-version-regexp-list :around 'ignore)"	\
	--eval "(advice-add 'package-lint--check-symbol-separators :around 'ignore)"	\
	--eval "(advice-add 'package-lint--check-defs-prefix :around 'ignore)"		\
	--eval "(advice-add 'package-lint--check-provide-form :around 'ignore)"		\
	-f package-lint-batch-and-exit fussy.el
