# Goal: Test .SECONDARY special target with pattern rules
# Subtility: .SECONDARY prevents deletion of intermediate files

all: file.out

# Mark .tmp files as secondary (not deleted)
.SECONDARY: %.tmp

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
# check if file.tmp still exists (not deleted because of .SECONDARY)
test:
	@if [ -f file.out ] && [ -f file.tmp ]; then \
		cat file.out | grep "This is a test file." > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
