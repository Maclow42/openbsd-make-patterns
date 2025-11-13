# 11-empty-dependencies-with-double-colon
# Test :: rule with no dependencies (catch-all pattern)

# ========== TEST RULES ==========
all:

file.out:

%::
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f all ]; then \
		if grep -q "Test passed." all 2>/dev/null; then \
			echo "[OK]"; \
		else \
			echo "[KO]"; \
		fi; \
	else \
		echo "[KO]"; \
	fi

clean:
	@rm -f `ls | grep -v Makefile`
