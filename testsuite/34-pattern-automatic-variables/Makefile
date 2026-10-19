# 34-pattern-automatic-variables
# Test $@ (target) and $< (first prerequisite) in the commands of a
# pattern rule (see test 39 for $*)

# ========== TEST RULES ==========
all: foo.out

%.out: %.in %.dep
	@echo "at=$@ lt=$<" > $@

# ========== INPUT FILES ==========
foo.in foo.dep:
	@touch $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f foo.out ] && grep -qx "at=foo.out lt=foo.in" foo.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
