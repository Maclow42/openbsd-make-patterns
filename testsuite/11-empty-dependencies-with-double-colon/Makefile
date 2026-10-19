# 11-empty-dependencies-with-double-colon
# Test :: rule with no dependencies (catch-all pattern): the target all
# has no commands, so the match-anything rule %:: builds a file named all

# ========== TEST RULES ==========
all:

file.out:

%::
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
.PHONY: clean test

test:
	@if [ -f all ] && grep -q "Test passed." all; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
