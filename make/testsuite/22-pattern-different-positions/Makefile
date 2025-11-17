# 22-pattern-different-positions
# Test pattern % in different positions

# ========== TEST RULES ==========
all: prefix_file_suffix.out

prefix_%_suffix.out: prefix_%_suffix.in
	@cat prefix_file_suffix.in > $@

# ========== INPUT FILES ==========
prefix_file_suffix.in:
	@echo "Test passed." > $@
	
# ========== VALIDATION RULES ==========
test:
	@if [ -f prefix_file_suffix.out ] && grep -q "Test passed." prefix_file_suffix.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f `ls | grep -v Makefile`
