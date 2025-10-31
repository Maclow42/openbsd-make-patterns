# Goal: Test pattern rules combined with pattern substitution references
# Subtility: $(var:pattern=replacement) with pattern rules

SOURCES = file1.in file2.in file3.in
OUTPUTS = $(SOURCES:.in=.out)

all: $(OUTPUTS)

%.out: %.in
	cp $*.in $@
	echo " processed" >> $@

clean:
	rm -f $(OUTPUTS)

# ----- test -----
.PHONY: test

# check if all output files are created
test:
	@if [ -f file1.out ] && [ -f file2.out ] && [ -f file3.out ]; then \
		cat file1.out | grep "Test 1" > /dev/null && \
		cat file2.out | grep "Test 2" > /dev/null && \
		cat file3.out | grep "Test 3" > /dev/null && \
		echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
