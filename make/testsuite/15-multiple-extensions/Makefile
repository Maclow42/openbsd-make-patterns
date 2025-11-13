# 16-multiple-extensions
# Test pattern rules with multiple file extensions

# ========== TEST RULES ==========
all: file1.out file2.out

file1.out: file.tmp1
	@echo "Test passed." > $@

file2.out: file.tmp2
	@echo "Test passed." > $@

%.tmp1: %.in
	@echo "temp" > $@

%.tmp2: %.in
	@echo "temp" > $@

# ========== INPUT FILES ==========
file.in:
	@echo "input" > $@

# ========== VALIDATION RULES ==========
test:
	@ok=0; \
	if [ -f file1.out ] && grep -q "Test passed." file1.out; then \
		ok=`expr $$ok + 1`; \
	fi; \
	if [ -f file2.out ] && grep -q "Test passed." file2.out; then \
		ok=`expr $$ok + 1`; \
	fi; \
	if [ $$ok -eq 2 ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f `ls | grep -v Makefile`
