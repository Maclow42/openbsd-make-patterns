# 57-pattern-not-default-target
# Test that, as in GNU make, a pattern rule given first is not the
# default target: default.mk, run with no target, starts with a pattern
# rule and must build its next target, file.out

# ========== TEST RULES ==========
all:
	@printf '%%.out: %%.in\n\t@cat $$< > $$@\n\nfirst: file.out\n\nfile.in:\n\t@echo "Test passed." > $$@\n' > default.mk
	@$(MAKE) -s -f default.mk

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
