# Goal: Test pattern-specific variables
# Subtility: Variables can be set for all targets matching a pattern

all: file.out test.out

# Pattern-specific variable
%.out: SUFFIX = processed

%.out: %.in
	cp $*.in $@
	echo " $(SUFFIX)" >> $@

clean:
	rm -f file.out test.out

# ----- test -----
.PHONY: test

# check if both output files contain "processed"
test:
	@if [ -f file.out ] && [ -f test.out ]; then \
		cat file.out | grep "processed" > /dev/null && \
		cat test.out | grep "processed" > /dev/null && \
		echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
