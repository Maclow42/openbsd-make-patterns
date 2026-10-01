# 04-multiple-patterns-in-target
# Test a prerequisite holding two %: as in GNU make, only the first one
# is replaced by the stem, so file.out depends on file-%.in

# ========== TEST RULES ==========
all: file.out

%.out: %-%.in
	@cp $< $@

# ========== INPUT FILES ==========
file-%.in:
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
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
