# 13-priority-multiple-patterns
# Test priority when multiple pattern rules match
# GNU Make: last rule wins
# OpenBSD Make: first rule wins

# ========== TEST RULES ==========
all: file.out

%.out:
	@echo "First rule" > $@

%.out:
	@echo "Second rule" > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && grep -q "First rule" file.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f `ls | grep -v Makefile`
