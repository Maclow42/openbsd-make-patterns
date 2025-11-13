# 17-subdirectory-patterns
# Test pattern rules with subdirectories

# ========== TEST RULES ==========
all: sub/file.out

sub/%.out: sub/%.in
	@echo "Test passed." > $@

# ========== INPUT FILES ==========
sub/file.in:
	@mkdir -p sub
	@echo "input" > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f sub/file.out ] && grep -q "Test passed." sub/file.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v Makefile`
