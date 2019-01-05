EMACS ?= emacs
SELECTOR ?= t

TESTS ?= $(wildcard *test.el)

test:
	$(EMACS) -Q --batch  -L . \
		-l subr \
	    $(addprefix -l ,$(TESTS)) \
		--eval '(setq ert-batch-backtrace-right-margin 100)' \
 		--eval '(ert-run-tests-batch-and-exit (quote $(SELECTOR)))'

