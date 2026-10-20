# 53-double-colon-different-prerequisites
# Test that, as in GNU make, :: pattern rules with different prerequisites
# do not replace each other: the first one wins

# ========== TEST RULES ==========
all: file.out

%.out:: %.a
	@echo "first" > $@

%.out:: %.b
	@echo "second" > $@

# ========== INPUT FILES ==========
file.a file.b:
	@touch $@

# ========== VALIDATION RULES ==========
.PHONY: all clean test

test:
	@if [ -f file.out ] && grep -qx "first" file.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
