# 08-basic-multiple-targets
# Test single rule generating multiple targets simultaneously

# ========== TEST RULES ==========
all: file.out

file.out: file1.tmp file2.tmp
	@cat file1.tmp file2.tmp > $@

%1.tmp %2.tmp:
	@echo "Test" > file1.tmp
	@echo "passed." > file2.tmp

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && [ -f file1.tmp ] && [ -f file2.tmp ]; then \
		if grep -q "Test" file.out && grep -q "passed" file.out; then \
			echo "[OK]"; \
			exit 0; \
		fi; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f `ls | grep -v Makefile`
