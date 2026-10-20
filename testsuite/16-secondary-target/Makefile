# 16-secondary-target
# Test .SECONDARY special target: file.tmp is kept after the build, but
# it is still an intermediate file, so once removed a second run does
# not make it again

# ========== TEST RULES ==========
all:
	@$(MAKE) -s build > /dev/null 2>&1
	@cp builds builds.first
	@[ -f file.tmp ] && touch kept
	@rm -f file.tmp
	@$(MAKE) -s build > /dev/null 2>&1

build: file.out

.SECONDARY: file.tmp

%.out: %.tmp
	@cat $< > $@
	@echo $@ >> builds

%.tmp: %.in
	@cat $< > $@
	@echo $@ >> builds

# ========== INPUT FILES ==========
file.in:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && grep -q "Test passed." file.out && \
	    [ -f kept ] && [ ! -f file.tmp ] && \
	    cmp -s builds.first builds && \
	    [ "`cat builds | tr '\n' ' '`" = "file.tmp file.out " ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
