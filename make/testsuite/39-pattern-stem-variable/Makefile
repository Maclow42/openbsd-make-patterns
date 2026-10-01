# 39-pattern-stem-variable
# Test $* in a pattern rule: it is the stem, with or without
# prerequisites. The extensions are not in .SUFFIXES, where BSD make
# would also strip them from $*

# ========== TEST RULES ==========
all: file.res

file.res: file.tmp
	@cat file.tmp > $@

%.tmp: %.src
	@echo "with=$*" > $@
	@cat $< >> $@

%.src:
	@echo "without=$*" > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.res ] && grep -qx "with=file" file.res && \
	    grep -qx "without=file" file.res; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
