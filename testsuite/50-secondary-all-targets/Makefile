# 50-secondary-all-targets
# Test .SECONDARY with no prerequisites: no intermediate file is removed,
# so file.tmp and file.mid, made by pattern rules, are kept

# ========== TEST RULES ==========
all: file.out

.SECONDARY:

%.out: %.mid
	@cat $< > $@

%.mid: %.tmp
	@cat $< > $@

%.tmp: %.in
	@cat $< > $@

# ========== INPUT FILES ==========
file.in:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && grep -q "Test passed." file.out && \
	    [ -f file.tmp ] && [ -f file.mid ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
