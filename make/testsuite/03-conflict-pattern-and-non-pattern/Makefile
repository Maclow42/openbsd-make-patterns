# 03-conflict-pattern-and-non-pattern
# Test that explicit rule takes precedence over pattern rule

# ========== TEST RULES ==========
all: file.out

%.out: file.in
	@echo "Pattern rule" > $@

file.out: file.in
	@echo "Test passed." > $@

# ========== INPUT FILES ==========
file.in:
	@echo "input" > $@

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
