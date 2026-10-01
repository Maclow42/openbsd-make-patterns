# 31-explicit-rule-without-prerequisites
# Test that an explicit rule with commands but no prerequisites takes
# precedence over a matching pattern rule, as in GNU make (see test 03
# for an explicit rule with prerequisites)

# ========== TEST RULES ==========
all: file.out

file.out:
	@echo "Test passed." > $@

%.out:
	@echo "Pattern rule" > $@
	@echo "Pattern rule" >> pattern.log

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && grep -q "Test passed." file.out && \
	    [ ! -f pattern.log ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
