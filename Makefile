# Development tasks.  Run `make' to check everything, as the CI does.
#
#   make compile   byte-compile, warnings are errors
#   make lint      package-lint, the MELPA rules
#   make relint    the regular expressions and the docstring escapes
#   make test      ERT test suite
#   make clean     remove build output and the tool sandbox
#
# The indent, checkdoc and complexity checks are pre-commit hooks of
# https://github.com/MArpogaus/elisp-complexity, not targets here.
#
# The checks install their tools and this package's dependencies into
# $(SANDBOX), so a fresh checkout needs nothing but Emacs and make.

EMACS   ?= emacs
SANDBOX ?= .sandbox
# The sandbox is done when the stamp is there: a run that dies half
# way leaves the directory behind, and a directory target would then
# count as made and the tools stay missing.
STAMP   := $(SANDBOX)/.installed
DEPS    ?= package-lint relint

SRC  := $(filter-out %-autoloads.el %-pkg.el,$(wildcard *.el))
TEST := $(wildcard test/*.el)

# Elisp programs live in variables: make joins their continuation lines,
# while a backslash inside a quoted recipe line would reach Emacs as is.
init = (progn (setq package-user-dir (expand-file-name "$(SANDBOX)")) \
              (require (quote package)) \
              (add-to-list (quote package-archives) \
                           (cons "melpa" "https://melpa.org/packages/") t) \
              (package-initialize))
bootstrap = (progn (package-refresh-contents) \
                   (dolist (p (quote ($(DEPS)))) \
                     (unless (package-installed-p p) (package-install p))))

BATCH = $(EMACS) -Q --batch -L . -L test --eval '$(init)'

.PHONY: all compile lint relint test clean

all: compile lint relint test

$(STAMP):
	@$(EMACS) -Q --batch --eval '$(init)' --eval '$(bootstrap)'
	@touch $@

compile: $(STAMP)
	@$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRC) $(TEST)
	@rm -f ./*.elc test/*.elc

lint: $(STAMP)
	@$(BATCH) -f package-lint-batch-and-exit $(SRC)

# What checkdoc and package-lint both let through: a docstring escape
# written \= rather than \\=, which the reader eats, so `describe-function'
# shows the wrong thing.
relint: $(STAMP)
	@$(BATCH) -l relint -f relint-batch $(SRC) $(TEST)

test: $(STAMP)
	@$(BATCH) $(addprefix -l ,$(TEST)) -f ert-run-tests-batch-and-exit

clean:
	@rm -rf $(SANDBOX) ./*.elc test/*.elc
