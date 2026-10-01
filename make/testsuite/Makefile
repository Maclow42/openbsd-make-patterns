# Global Makefile to run all tests
#
# A test passes if its `make test` exits with status 0 and prints [OK].
#
# A test directory holding a KNOWN_FAILURE file is a known difference
# between this fork and GNU make; the file says why. Such a test is
# expected to fail with this fork: its failure is reported as [XFAIL]
# and does not fail the run, while an unexpected success is reported
# as [XPASS] and does fail it, so that the KNOWN_FAILURE file gets
# removed. With GNU make (MAKE_VERSION is only defined by GNU make),
# KNOWN_FAILURE files are ignored: GNU make is the reference and must
# pass every test.

TEST_DIRS = find . ! -path ./Makefile -name Makefile -exec dirname {} \;|sort -u

.PHONY: all test clean

all: test

test: clean
	@echo "=== Running Test Suite ==="
	@set +e; passed=0; failed=0; xfailed=0; xpassed=0; \
	case "$(MAKE_VERSION)" in "") known=yes;; *) known=no;; esac; \
	$(TEST_DIRS)|{ while read dir; do \
		printf "Testing $$dir ... "; \
		$(MAKE) -s -C $$dir clean > /dev/null 2>&1; \
		$(MAKE) -s -C $$dir all > /dev/null 2>&1; \
		if out=`$(MAKE) -s -C $$dir test 2>&1` && \
		    echo "$$out" | grep -qx '\[OK\]'; then \
			ok=yes; \
		else \
			ok=no; \
		fi; \
		if [ $$known = yes ] && [ -f $$dir/KNOWN_FAILURE ]; then \
			case $$ok in \
			yes) echo "[XPASS] (remove $$dir/KNOWN_FAILURE)"; \
			    xpassed=$$(( $$xpassed + 1));; \
			no) echo "[XFAIL] `head -1 $$dir/KNOWN_FAILURE`"; \
			    xfailed=$$(( $$xfailed + 1));; \
			esac; \
		else \
			case $$ok in \
			yes) echo "[OK]"; passed=$$(( $$passed + 1));; \
			no) echo "[KO]"; failed=$$(( $$failed + 1));; \
			esac; \
		fi; \
	    done; \
	    echo ""; \
	    echo "=== Test Summary ==="; \
	    echo "Passed:          $$passed"; \
	    echo "Failed:          $$failed"; \
	    echo "Known failures:  $$xfailed"; \
	    echo "Unexpected pass: $$xpassed"; \
	    total=$$(( $$passed + $$failed + $$xfailed + $$xpassed )); \
	    echo "Total:           $$total"; \
	    case $$failed$$xpassed in 00) ;; *) exit 1;; esac; \
	}

clean:
	@$(TEST_DIRS)|while read dir; do \
		$(MAKE) -C $$dir clean > /dev/null 2>&1 || true; \
	done
	@echo "All test directories cleaned"
