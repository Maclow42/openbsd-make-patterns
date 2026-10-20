# 29-pattern-rule-up-to-date
# Test that a second run does nothing: as in GNU make, an intermediate
# file removed after the first run does not make its target out of date

# ========== TEST RULES ==========
all:
	@$(MAKE) -s build > /dev/null 2>&1
	@cp builds builds.first
	@$(MAKE) -s build > /dev/null 2>&1

build: file.out

%.out: %.tmp
	@cp $< $@
	@echo $@ >> builds

%.tmp: %.in
	@cp $< $@
	@echo $@ >> builds

# ========== INPUT FILES ==========
file.in:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && grep -q "Test passed." file.out && \
	    [ ! -f file.tmp ] && cmp -s builds builds.first; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
