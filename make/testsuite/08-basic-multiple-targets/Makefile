# 08-basic-multiple-targets
# Test single rule generating multiple targets simultaneously: as in GNU
# make, one run of the recipe builds both targets

# ========== TEST RULES ==========
all: file.out

file.out: file1.tmp file2.tmp
	@cat file1.tmp file2.tmp > $@

%1.tmp %2.tmp:
	@echo "Test" > file1.tmp
	@echo "passed." > file2.tmp
	@echo "run" >> runs

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && [ -f file1.tmp ] && [ -f file2.tmp ] && \
	    grep -q "Test" file.out && grep -q "passed" file.out && \
	    [ `wc -l < runs` -eq 1 ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
