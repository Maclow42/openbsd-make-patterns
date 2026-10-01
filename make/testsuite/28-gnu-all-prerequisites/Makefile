# 28-gnu-all-prerequisites
# Test GNU make's $^ automatic variable (all prerequisites)

# ========== TEST RULES ==========
all: many.out single.out

many.out: a.src b.src c.src
	@echo "$^" > $@

single.out: a.src
	@echo "$^" > $@

# ========== INPUT FILES ==========
a.src b.src c.src:
	@touch $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f many.out ] && [ -f single.out ] && \
	    grep -q '^a.src b.src c.src$$' many.out && \
	    grep -q '^a.src$$' single.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f `ls | grep -v Makefile`
