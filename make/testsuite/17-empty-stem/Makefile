# 18-empty-stem
# Test pattern rule with minimal stem (single character)

# ========== TEST RULES ==========
all: file_x.out

file_%.out: file_%.in
	@echo "Test passed." > $@

# ========== INPUT FILES ==========
file_x.in:
	@echo "input" > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file_x.out ] && grep -q "Test passed." file_x.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f `ls | grep -v Makefile`
