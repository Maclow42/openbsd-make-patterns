# 02-basic-only-target
# Test pattern rule with explicit dependency (%.out: file.in)

# ========== TEST RULES ==========
all: file.out

%.out: file.in
	@cp $? $@

# ========== INPUT FILES ==========
file.in:
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
