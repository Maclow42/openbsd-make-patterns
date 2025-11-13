# 14-automatic-variables
# Test automatic variables in pattern rules: $@, $<, $*, $^

# ========== TEST RULES ==========
all: file.out

%.out: %1.in %2.in
	@echo "Target: $@" > $@
	@echo "First: $<" >> $@
	@echo "Stem: $*" >> $@
	@echo "All: $^" >> $@

# ========== INPUT FILES ==========
file1.in:
	@echo "input" > $@

file2.in:
	@echo "input" > $@

# ========== VALIDATION RULES ==========
test:
	@ok=0; \
	if [ -f file.out ]; then \
		if grep -q "Target: file.out" file.out; then ok=`expr $$ok + 1`; fi; \
		if grep -q "First: file1.in" file.out; then ok=`expr $$ok + 1`; fi; \
		if grep -q "Stem: file" file.out; then ok=`expr $$ok + 1`; fi; \
		if grep -q "All: file1.in file2.in" file.out; then ok=`expr $$ok + 1`; fi; \
	fi; \
	if [ $$ok -eq 4 ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f `ls | grep -v Makefile`
