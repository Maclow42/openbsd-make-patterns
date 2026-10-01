# 24-pattern-specific-variables
# Test pattern-specific variables

# ========== TEST RULES ==========
all: file.out

%.out: FLAGS = ok
%.out: %.in
	@if [ "$(FLAGS)" = "ok" ]; then cat $< > $@; else echo "failed" > $@; fi

# ========== INPUT FILES ==========
file.in:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && grep -q "Test passed." file.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
