EMACS=emacs

.PHONY: test

TEST_SRCS = $(wildcard test-data/*)

test: compile-all checkdoc
	$(EMACS) -Q --batch $(CL) \
	    --eval '(setq byte-compile-warnings (quote (not cl-functions)))' \
	    -l ert -l ../format-table.elc -l format-table-test.elc \
	    --eval '(ert-run-tests-batch-and-exit $(SELECTOR))'

checkdoc:
	$(EMACS) -Q --batch $(CL) -l checkdoc-batch.el 2>&1 | \
	    grep -E "format-table.el:[1-9]+" && exit 1 || exit 0

compile-all:
	$(EMACS) -Q --batch $(CL) \
	    --eval '(setq byte-compile-warnings (quote (not cl-functions)))' \
	    --eval '(setq byte-compile-error-on-warn t)' \
	    -l ../format-table.el \
	    -f batch-byte-compile ../format-table.el format-table-test.el

html: $(TEST_HTML)

clean:
	rm -f $(TEST_HTML) *.elc ../*.elc

evm:
	$(MAKE) test EMACS="$(shell evm bin emacs-25.1)" CL=""
	$(MAKE) test EMACS="$(shell evm bin emacs-25.2)" CL=""
	$(MAKE) test EMACS="$(shell evm bin emacs-25.3)" CL=""
	$(MAKE) test EMACS="$(shell evm bin emacs-26.1)" CL=""
