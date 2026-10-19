# 30-shortest-stem
# Test that, as in GNU make, the pattern rule with the shortest stem wins
# when several match, whatever their order: f%.out over %.out for foo.out,
# b%.res over %.res for bar.res

# ========== TEST RULES ==========
all: foo.out bar.res

%.out: %.in
	@echo "generic" > $@

f%.out: f%.in
	@echo "specific" > $@

b%.res: b%.in
	@echo "specific" > $@

%.res: %.in
	@echo "generic" > $@

# ========== INPUT FILES ==========
foo.in bar.in:
	@touch $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f foo.out ] && grep -qx "specific" foo.out && \
	    [ -f bar.res ] && grep -qx "specific" bar.res; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
