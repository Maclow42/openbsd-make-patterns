# 02-basic-only-target
# Test pattern rule with %.out: %.in

# ========== TEST RULES ==========
all: file.out

%.out: %.in
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
