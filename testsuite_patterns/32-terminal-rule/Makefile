# Goal: Test .NOTINTERMEDIATE (or terminal) pattern rules
# Subtility: Marks files as not intermediate even if implicitly created

all: file.out

# Mark .tmp files as not intermediate (keep them)
.NOTINTERMEDIATE: %.tmp

%.out: %.tmp
	cp $*.tmp $@
	echo " final" >> $@

%.tmp: %.in
	cp $*.in $@
	echo " intermediate" >> $@

clean:
	rm -f file.out file.tmp

# ----- test -----
.PHONY: test

# check if file.out is created
# check if file.tmp still exists (marked as not intermediate)
test:
	@if [ -f file.out ] && [ -f file.tmp ]; then \
		cat file.out | grep "This is a test file." > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
