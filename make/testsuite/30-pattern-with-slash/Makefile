# 31-pattern-with-slash
# Test pattern rules with explicit slash in pattern

# ========== TEST RULES ==========
all: output/file.out

output/%.out: input/%.in
	@mkdir -p output
	@echo "Test passed." > $@

# ========== INPUT FILES ==========
input/file.in:
	@mkdir -p input
	@echo "input" > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f output/file.out ] && grep -q "Test passed." output/file.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v Makefile`
