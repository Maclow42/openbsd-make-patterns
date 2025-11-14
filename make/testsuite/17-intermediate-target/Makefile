# 19-intermediate-target
# Test .INTERMEDIATE special target to delete intermediate files
# INTERMEDIATE doest not understand pattern rules

# ========== TEST RULES ==========
all: file.out

.INTERMEDIATE: file.in

%.out: %.in
	@cat file.in > $@

# ========== INPUT FILES ==========
file.in:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
.PHONY: test

test:
	@if [ -f file.out ] && grep -q "Test passed." file.out; then \
		if [ ! -f file.in ]; then \
			echo "[OK]"; \
			exit 0; \
		fi; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f `ls | grep -v Makefile`
