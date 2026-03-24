# 27-gnu-shell-function-tricky
# Validate tricky GNU function $(shell ...) expansions.

INNER = nested
PAREN = left(right)

all: tricky1.out tricky2.out

tricky1.out:
	@printf "%s\n" "$(shell printf $(shell printf $(INNER))_ok)" > $@

tricky2.out:
	@printf "%s\n" "$(shell printf '%s' '$(PAREN)')" > $@

test:
	@if [ -f tricky1.out ] && [ -f tricky2.out ] && \
	    grep -q '^nested_ok$$' tricky1.out && \
	    grep -q '^left(right)$$' tricky2.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f tricky1.out tricky2.out
