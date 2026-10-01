# 10-simple-op-double-colon
# Test pattern rule with :: operator (last definition wins for patterns)

# ========== TEST RULES ==========
all: file.out

%.out::
	@touch $@

%.out::
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
.PHONY: all clean test

test:
	@if [ -f file.out ] && grep -q "Test passed." file.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
