# 28-gnu-all-prerequisites
# Test GNU make's $^ automatic variable (all prerequisites, in order and
# without duplicates)

# ========== TEST RULES ==========
all: many.out single.out dup.out

many.out: a.src b.src c.src
	@echo "$^" > $@

single.out: a.src
	@echo "$^" > $@

dup.out: a.src b.src a.src
	@echo "$^" > $@

# ========== INPUT FILES ==========
a.src b.src c.src:
	@touch $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f many.out ] && [ -f single.out ] && [ -f dup.out ] && \
	    grep -q '^a.src b.src c.src$$' many.out && \
	    grep -q '^a.src$$' single.out && \
	    grep -q '^a.src b.src$$' dup.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
