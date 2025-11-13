# 27-pattern-with-phony
# Test pattern rule combined with .PHONY targets

# ========== TEST RULES ==========
.PHONY: all clean test

all: file.out

%.out: %.in
	@echo "Test passed." > $@

# ========== INPUT FILES ==========
file.in:
	@echo "input" > $@

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
