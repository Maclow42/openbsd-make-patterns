# 42-intermediate-needed-after-all
# Test that an intermediate file removed after the first run is made
# again when its target is out of date because of another prerequisite:
# the target's commands need it

# ========== TEST RULES ==========
all:
	@$(MAKE) -s build > /dev/null 2>&1
	@sleep 1
	@echo "Test passed." > extra.h
	@$(MAKE) -s build > /dev/null 2>&1

build: file.out

%.out: %.tmp extra.h
	@cat file.tmp extra.h > $@
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
	    [ ! -f file.tmp ] && \
	    [ "`cat builds | tr '\n' ' '`" = \
	    "file.tmp file.out file.tmp file.out " ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
