# 01-basic-pattern
# Test basic pattern rule: %.out: (no prerequisites)

# ========== TEST RULES ==========
all: file.out

%.out:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ]; then \
		if grep -q "Test passed." file.out; then \
			echo "[OK]"; \
			exit 0; \
		fi; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f `ls | grep -v Makefile`
