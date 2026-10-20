# 46-intermediate-parallel-second-run
# Test tests 41 and 42 in parallel mode (-j4): once the intermediate
# files are removed, a second run does nothing, and once the target is
# out of date because of another prerequisite, a third run makes the
# chain of intermediate files again before the target

# ========== TEST RULES ==========
all:
	@$(MAKE) -s -j4 build > /dev/null 2>&1
	@cp builds builds.first
	@$(MAKE) -s -j4 build > /dev/null 2>&1
	@cp builds builds.second
	@sleep 1
	@echo "Test passed." > extra.h
	@$(MAKE) -s -j4 build > /dev/null 2>&1

build: file.out

%.out: %.mid extra.h
	@cat file.mid extra.h > $@
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

extra.h:
	@echo "Old header." > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && [ `grep -c "Test passed." file.out` -eq 2 ] && \
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
