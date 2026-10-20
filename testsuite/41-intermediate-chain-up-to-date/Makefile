# 41-intermediate-chain-up-to-date
# Test a chain of two intermediate files, both removed after the first
# run: a second run does nothing, and once the source changes a third
# run rebuilds the whole chain again

# ========== TEST RULES ==========
all:
	@$(MAKE) -s build > /dev/null 2>&1
	@cp builds builds.first
	@$(MAKE) -s build > /dev/null 2>&1
	@cp builds builds.second
	@sleep 1
	@touch file.in
	@$(MAKE) -s build > /dev/null 2>&1

build: file.out

%.out: %.mid
	@cp $< $@
	@echo $@ >> builds

%.mid: %.tmp
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
	    [ ! -f file.tmp ] && [ ! -f file.mid ] && \
	    cmp -s builds.first builds.second && \
	    [ "`cat builds | tr '\n' ' '`" = \
	    "file.tmp file.mid file.out file.tmp file.mid file.out " ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
