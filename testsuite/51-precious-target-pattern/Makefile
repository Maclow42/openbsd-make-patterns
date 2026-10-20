# 51-precious-target-pattern
# Test .PRECIOUS given the target pattern of a rule: the intermediate
# files that rule makes are kept after a successful build, the others
# are still removed

# ========== TEST RULES ==========
all: file.out

.PRECIOUS: %.tmp

%.out: %.mid
	@cat $< > $@

%.mid: %.tmp
	@cat $< > $@

%.tmp: %.in
	@cat $< > $@

# ========== INPUT FILES ==========
file.in:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && grep -q "Test passed." file.out && \
	    [ -f file.tmp ] && [ ! -f file.mid ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
