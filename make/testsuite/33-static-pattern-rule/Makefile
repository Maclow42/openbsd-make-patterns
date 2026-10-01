# 33-static-pattern-rule
# Test static pattern rules: the pattern only applies to the listed
# targets (targets: target-pattern: prerequisite-patterns)

# ========== TEST RULES ==========
OUTPUTS = file1.out file2.out

all: $(OUTPUTS)

$(OUTPUTS): %.out: %.in
	@cat $< > $@

# ========== INPUT FILES ==========
file1.in file2.in:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file1.out ] && grep -q "Test passed." file1.out && \
	    [ -f file2.out ] && grep -q "Test passed." file2.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
