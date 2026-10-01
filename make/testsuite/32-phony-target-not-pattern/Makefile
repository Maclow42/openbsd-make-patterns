# 32-phony-target-not-pattern
# Test that, as in GNU make, a .PHONY target is never built through a
# pattern rule, even a match-anything one

# ========== TEST RULES ==========
.PHONY: all phony

all: phony

phony:

%:
	@touch $@

# ========== VALIDATION RULES ==========
.PHONY: test clean

test:
	@if [ ! -f phony ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
