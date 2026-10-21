# 56-pattern-rule-attribute-line
# Test that a dependency line giving only an attribute to a pattern
# target, before or after its rule, is not taken for a rule of its own

# ========== TEST RULES ==========
all: file.out file.res

%.out: .PRECIOUS

%.out: %.in
	@cat $< > $@

%.res: %.in
	@cat $< > $@

%.res: .PRECIOUS

# ========== INPUT FILES ==========
file.in:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && grep -q "Test passed." file.out && \
	    [ -f file.res ] && grep -q "Test passed." file.res; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
