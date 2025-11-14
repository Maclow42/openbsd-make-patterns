# 12-dir-as-pattern
# Test pattern with % in directory path

# ========== TEST RULES ==========
all: file.out

%.out: ./src/tmp/%.tmp
	@cat src/tmp/file.tmp > $@

%.tmp:
	@mkdir -p src/tmp
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
	@rm -rf `ls | grep -v Makefile`
