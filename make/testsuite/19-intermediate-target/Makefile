# 20-intermediate-target
# Test .INTERMEDIATE special target to delete intermediate files

# ========== TEST RULES ==========
all: file.out

.INTERMEDIATE: file.tmp

%.out: %.tmp
	@echo "Test passed." > $@

%.tmp: %.in
	@echo "temp" > $@

# ========== INPUT FILES ==========
file.in:
	@echo "input" > $@

# ========== VALIDATION RULES ==========
.PHONY: test

test:
	@$(MAKE) -s all > /dev/null 2>&1; \
	if [ -f file.out ] && grep -q "Test passed." file.out; then \
		if [ ! -f file.tmp ]; then \
			echo "[OK]"; \
			exit 0; \
		fi; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f `ls | grep -v Makefile`
