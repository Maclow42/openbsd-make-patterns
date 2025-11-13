# 26-empty-recipe
# Test pattern rule with empty recipe

# ========== TEST RULES ==========
all: file.out

%.out: %.in
	@

file.in:
	@echo "input" > $@

file.out: file.in
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
	@rm -f `ls | grep -v Makefile`
