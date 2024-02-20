export EMACS ?= $(shell which emacs)

ELFILES = process-history.el
ELCFILES = $(addsuffix .elc, $(basename $(ELFILES)))

all: $(ELCFILES)

%.elc: %.el
	@echo Compiling $<
	@${EMACS} -Q \
	          -batch \
                  -no-site-file \
                  -L . \
                  -f batch-byte-compile $<

clean:
	@rm -f *.elc
