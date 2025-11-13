# 04-multiple-patterns-in-target
# Test multiple pattern rules for different extensions
# Goal: observe behavior of %-%.in (the second % should be useless)

# ========== TEST RULES ==========
all: file.out

%.out: %-%.in
	@cp file-%.in $@

# ========== INPUT FILES ==========
file-%.in:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@if grep -q "Test passed." file.out 2>/dev/null; then \
		echo "[OK]"; \
	else \
		echo "[KO]"; \
	fi

clean:
	@rm -f `ls | grep -v Makefile`

