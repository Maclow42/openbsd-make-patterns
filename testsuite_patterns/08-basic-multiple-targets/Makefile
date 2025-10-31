# Goal: use a single rule to generate two dependencies
# file1.tmp and file2.tmp are simultaneously generated from file.sh

all: file.out

file.out: file1.tmp file2.tmp
	cat file1.tmp file2.tmp > $@

%1.tmp %2.tmp: %.sh
	ksh $>

clean:
	rm -f file.out file1.tmp file2.tmp

# ----- test -----
.PHONY: test

test:
	@if [ -f file.out ] && [ -f file1.tmp ] && [ -f file2.tmp ]; then \
		if grep -q "This is a test file." file.out 2>/dev/null; then \
			echo "[OK]"; \
		else \
			echo "[KO]"; \
		fi; \
	else \
		echo "[KO]"; \
	fi
