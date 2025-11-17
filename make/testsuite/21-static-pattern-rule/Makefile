# 21-static-pattern-rule
# Test static pattern rules

# ========== TEST RULES ==========
OUTPUTS = file1.out file2.out

all: $(OUTPUTS)

%.out: %.in
	@cat $? > $@

# ========== INPUT FILES ==========
file1.in:
	@echo "Test passed." > $@

file2.in:
	@echo "Test passed." > $@

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
