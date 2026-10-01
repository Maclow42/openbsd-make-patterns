# 13-priority-multiple-patterns
# Test priority when multiple pattern rules match
# A pattern rule with the same target and prerequisites as an existing
# one replaces it, as in GNU make: the last rule wins.

# ========== TEST RULES ==========
all: file.out

%.out:
	@echo "First rule" > $@

%.out:
	@echo "Second rule" > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && grep -q "Second rule" file.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f `ls | grep -v Makefile`
