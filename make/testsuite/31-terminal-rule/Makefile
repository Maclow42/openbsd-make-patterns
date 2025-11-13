# 32-terminal-rule
# Test .PRECIOUS to prevent deletion of intermediate files

# ========== TEST RULES ==========
all: file.out

.PRECIOUS: %.tmp

%.out: %.tmp
	@echo "Test passed." > $@

%.tmp: %.in
	@echo "temp" > $@

# ========== INPUT FILES ==========
file.in:
	@echo "input" > $@

# ========== VALIDATION RULES ==========
test:
	@ok=0; \
	if [ -f file.out ] && grep -q "Test passed." file.out; then \
		ok=`expr $$ok + 1`; \
	fi; \
	if [ -f file.tmp ] && grep -q "temp" file.tmp; then \
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
