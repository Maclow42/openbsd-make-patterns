# 38-gnu-shell-function-contexts
# Validate $(shell ...) in other contexts: immediate assignment, output on
# several lines (newlines become spaces), empty output, failing command,
# list of targets, and a variable whose name starts with "shell"

LINES := $(shell printf "a\nb\n")
EMPTY = $(shell true)
FAILED = $(shell false)
shellx = not-a-function

all: $(shell echo generated).out values.out

generated.out:
	@echo "Test passed." > $@

values.out:
	@echo "[$(LINES)] [$(EMPTY)] [$(FAILED)] [$(shellx)]" > $@

test:
	@if [ -f generated.out ] && grep -q "Test passed." generated.out && \
	    [ -f values.out ] && \
	    grep -qx '\[a b\] \[\] \[\] \[not-a-function\]' values.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f generated.out values.out
