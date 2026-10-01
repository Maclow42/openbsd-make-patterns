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
	@if [ -f file.out ]; then \
		if grep -q "Test passed." file.out 2>/dev/null; then \
			echo "[OK]"; \
		else \
			echo "[KO]"; \
		fi; \
	else \
		echo "[KO]"; \
	fi

clean:
	@rm -f `ls | grep -v Makefile`

