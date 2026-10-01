# Global Makefile to run all tests

PASSED = 0
FAILED = 0

TEST_DIRS = find . ! -path ./Makefile -name Makefile -exec dirname {} \;|sort -u

.PHONY: all test clean $(TEST_DIRS)

all: test

test: clean
	@echo "=== Running Pattern Rules Test Suite ==="
	@set +e; failed=0; passed=0; \
	$(TEST_DIRS)|{ while read dir; do \
		printf "Testing $$dir ... "; \
		$(MAKE) -s -C $$dir clean > /dev/null 2>&1; \
		$(MAKE) -s -C $$dir all > /dev/null 2>&1; \
		if out=`$(MAKE) -s -C $$dir test 2>&1` && \
		    echo "$$out" | grep -qx '\[OK\]'; then \
			echo "[OK]"; \
			passed=$$(( $$passed + 1)); \
		else \
			echo "[KO]"; \
			failed=$$(( $$failed + 1)); \
		fi; \
	    done; \
	    echo ""; \
	    echo "=== Test Summary ==="; \
	    echo "Passed: $$passed"; \
	    echo "Failed: $$failed"; \
	    total=$$(( $$passed + $$failed )); \
	    echo "Total:  $$total"; \
	    case $$failed in 0) ;; *) exit 1;; esac; \
	}

clean:
	@$(TEST_DIRS)|while read dir; do \
		$(MAKE) -C $$dir clean > /dev/null 2>&1 || true; \
	done
	@echo "All test directories cleaned"
