# Goal: Test .INTERMEDIATE special target with pattern rules
# Subtility: .INTERMEDIATE forces deletion of specified intermediate files

all: file.out

# Mark .tmp files as intermediate (will be deleted)
.INTERMEDIATE: %.tmp

%.out: %.tmp
	cp file.tmp file.out
	echo " final" >> file.out

%.tmp: %.in
	cp file.in file.tmp
	echo " intermediate" >> file.tmp

clean:
	rm -f file.out file.tmp

# ----- test -----
.PHONY: test

# check if file.out is created
# check if file.tmp has been deleted (because of .INTERMEDIATE)
test:
	@if [ -f file.out ] && [ ! -f file.tmp ]; then \
		cat file.out | grep "This is a test file." > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
