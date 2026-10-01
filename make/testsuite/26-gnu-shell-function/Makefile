# 26-gnu-shell-function
# Validate GNU function $(shell ...) expansion in variable substitution.

MSG = nested_ok

all: basic.out nested.out

basic.out:
	@printf "%s\n" "$(shell printf shell_basic)" > $@

nested.out:
	@printf "%s\n" "$(shell printf $(MSG))" > $@

test:
	@if [ -f basic.out ] && [ -f nested.out ] && \
	    grep -q '^shell_basic$$' basic.out && \
	    grep -q '^nested_ok$$' nested.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1


clean:
	@rm -f result-basic.out result-nested.out
