# 55-precious-pattern-several-rules
# Test that .PRECIOUS given after several rules of a pattern target
# applies to all of them: file.mid, built by the redefined %.mid rule,
# is kept

# ========== TEST RULES ==========
all: file.out

%.out: %.mid
	@cat $< > $@

%.mid: %.in
	@echo "First rule" > $@

%.mid: %.in
	@cat $< > $@

.PRECIOUS: %.mid

# ========== INPUT FILES ==========
file.in:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && [ -f file.mid ] && \
	    grep -q "Test passed." file.mid; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
