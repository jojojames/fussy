export EMACS ?= $(shell which emacs)
CASK_DIR := $(shell cask package-directory)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: compile
compile:
	eask compile

.PHONY: test
test:
	eask test ert ./fussy-test.el

.PHONY: lint
lint:
	eask lint package
