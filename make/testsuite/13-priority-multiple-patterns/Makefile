# 13-priority-multiple-patterns
# Test that more specific pattern has priority

# ========== TEST RULES ==========
all: file.special.out

%.out: file.in
	@echo "generic" > $@

%.special.out: file.in
	@echo "Test passed." > $@

# ========== INPUT FILES ==========
file.in:
	@echo "input" > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.special.out ] && grep -q "Test passed." file.special.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f `ls | grep -v Makefile`
