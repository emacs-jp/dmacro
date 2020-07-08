## Makefile

all:

REPO_USER    := emacs-jp
PACKAGE_NAME := dmacro
REPO_NAME    := dmacro

EMACS        ?= emacs

##################################################

.PHONY: all help build test lint clean

all: help

help:
	$(info )
	$(info Commands)
	$(info ========)
	$(info   - make          # Show this help)
	$(info   - make build    # Compile Elisp files)
	$(info   - make test     # Compile Elisp files and test $(PACKAGE_NAME))
	$(info   - make lint     # Lint Elisp files)
	$(info )
	$(info Cleaning)
	$(info ========)
	$(info   - make clean    # Clean compiled files)
	$(info )
	$(info This Makefile required `keg`)
	$(info See https://github.com/$(REPO_USER)/$(REPO_NAME)#contribution)
	$(info )

##############################

build:
	keg build

lint:
	keg lint

test: build
	keg exec $(EMACS) --batch -l $(PACKAGE_NAME)-tests.el -f cort-test-run

clean:
	keg clean
