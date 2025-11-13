# 12-dir-as-pattern
# Test pattern rule with directory in pattern

# ========== TEST RULES ==========
all: out/file.out

out/%.out: in/%.in
	@mkdir -p out
	@echo "Test passed." > $@

# ========== INPUT FILES ==========
in/file.in:
	@mkdir -p in
	@echo "input" > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f out/file.out ] && grep -q "Test passed." out/file.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v Makefile`
