# GNU Make Pattern Rules Test Suite
# Global Makefile to run all tests

TEST_DIRS = \
	01-basic-pattern \
	02-basic-only-target \
	03-conflict-pattern-and-non-pattern \
	04-multiple-patterns-in-target \
	05-multiple-not-linked-rules \
	06-multiple-linked-rules \
	07-multiple-linked-rules-del-temp \
	08-basic-multiple-targets \
	09-multiple-targets-del-temp \
	10-simple-op-double-colon \
	11-empty-dependencies-with-double-colon \
	12-dir-as-pattern \
	13-priority-multiple-patterns \
	14-automatic-variables \
	15-multiple-extensions \
	16-subdirectory-patterns \
	17-empty-stem \
	18-secondary-target \
	19-intermediate-target \
	20-order-only-prerequisites \
	21-vpath-pattern \
	22-double-extension \
	23-static-pattern-rule \
	24-pattern-different-positions \
	25-empty-recipe \
	26-pattern-with-phony \
	27-match-subst-reference \
	28-pattern-specific-variables \
	29-nested-patterns \
	30-pattern-with-slash \
	31-terminal-rule

PASSED = 0
FAILED = 0

.PHONY: all test clean $(TEST_DIRS)

all: test

test: clean
	@echo "=== Running Pattern Rules Test Suite ==="
	@sh -c ' \
	failed=0; passed=0; \
	for dir in $(TEST_DIRS); do \
		printf "Testing $$dir ... "; \
		$(MAKE) -s -C $$dir clean > /dev/null 2>&1; \
		$(MAKE) -s -C $$dir all > /dev/null 2>&1; \
		output=`$(MAKE) -s -C $$dir test 2>&1`; \
		if echo "$$output" | grep -q "^\[OK\]$$"; then \
			echo "[OK]"; \
			passed=`expr $$passed + 1`; \
		else \
			echo "[KO]"; \
			failed=`expr $$failed + 1`; \
		fi; \
	done; \
	echo ""; \
	echo "=== Test Summary ==="; \
	echo "Passed: $$passed"; \
	echo "Failed: $$failed"; \
	total=`expr $$passed + $$failed`; \
	echo "Total:  $$total"; \
	if [ $$failed -ne 0 ]; then \
		exit 1; \
	fi'

clean:
	@for dir in $(TEST_DIRS); do \
		$(MAKE) -C $$dir clean > /dev/null 2>&1 || true; \
	done
	@echo "All test directories cleaned"
