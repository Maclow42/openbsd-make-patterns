# 18-precious-target
# Test .PRECIOUS to prevent deletion of
# intermediate files when compilation failed

# ========== TEST RULES ==========
all: file.out

.PRECIOUS: file2.tmp

%.out: %1.tmp
	@failing_command

%1.tmp: %2.tmp
	@cat $? > $@

%2.tmp: %.in
	@cat $? > $@

# ========== INPUT FILES ==========
file.in:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test: all
	@ok=0; \
	if [ ! -f file.out ] && [ ! -f file1.tmp ]; then \
		ok=`expr $$ok + 1`; \
	fi; \
	if [ -f file2.tmp ] && grep -q "Test passed." file2.tmp; then \
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
