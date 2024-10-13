export EMACS ?= $(shell which emacs)

ELFILES = recall.el

ELCFILES = $(addsuffix .elc, $(basename $(ELFILES)))

all: $(ELCFILES)

%.elc: %.el
	@echo Compiling $<
	@${EMACS} -Q \
	          -batch \
                  -no-site-file \
                  -L . \
		  --eval '(setq byte-compile-error-on-warn t)' \
                  -f batch-byte-compile $<

clean:
	@rm -f *.elc
