# GNU Make Pattern Rules Test Suite
# Global Makefile to run all tests

TEST_DIRS != find . -mindepth 2 -type f -name Makefile -print \
              | sed 's|^\./||' | sed 's|/Makefile$$||' | sort -u

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
