# 23-match-subst-reference
# Test pattern rule with substitution references

# ========== TEST RULES ==========
INPUTS = file1.in file2.in
OUTPUTS = $(INPUTS:.in=.out)

.PHONY: all result

all: result

result: $(OUTPUTS)
	@touch result

%.out: %.in
	@cat $? > $@

%.in:
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
