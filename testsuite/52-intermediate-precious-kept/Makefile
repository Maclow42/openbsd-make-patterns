# 52-intermediate-precious-kept
# Test a file both .INTERMEDIATE and .PRECIOUS: as in GNU make, being
# precious, it is not removed after the build

# ========== TEST RULES ==========
all: file.out

.INTERMEDIATE: file.mid
.PRECIOUS: file.mid

file.out: file.mid
	@cat file.mid > $@

file.mid: file.in
	@cat file.in > $@

# ========== INPUT FILES ==========
file.in:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && grep -q "Test passed." file.out && \
	    [ -f file.mid ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
