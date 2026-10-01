# 17-intermediate-target
# Test .INTERMEDIATE special target to delete intermediate files: file.in
# has its own rule, but being .INTERMEDIATE it is removed after the build

# ========== TEST RULES ==========
all: file.out

.INTERMEDIATE: file.in

%.out: %.in
	@cat $< > $@

# ========== INPUT FILES ==========
file.in:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
.PHONY: test

test:
	@if [ -f file.out ] && grep -q "Test passed." file.out && \
	    [ ! -f file.in ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
