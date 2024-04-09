export EMACS ?= $(shell which emacs)

ELFILES = \
	process-history.el \
	extensions/process-history-consult.el \
	extensions/process-history-embark.el

ELCFILES = $(addsuffix .elc, $(basename $(ELFILES)))

all: $(ELCFILES)

%.elc: %.el
	@echo Compiling $<
	@${EMACS} -Q \
	          -batch \
                  -no-site-file \
                  -L . \
	          --eval="(package-initialize)" \
		  --eval="(package-refresh-contents)" \
	          --eval="(package-install 'consult)" \
	          --eval="(package-install 'embark)" \
		  --eval '(setq byte-compile-error-on-warn t)' \
                  -f batch-byte-compile $<

clean:
	@rm -f *.elc
