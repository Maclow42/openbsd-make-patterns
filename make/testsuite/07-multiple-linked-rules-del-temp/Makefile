# 07-multiple-linked-rules-del-temp
# Test chained pattern rules with intermediate file deletion

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
