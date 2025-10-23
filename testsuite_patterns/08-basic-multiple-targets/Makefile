# Goal: use a single rule to generate two dependencies
# Here, file1.tmp and file2.tmp are simulatenously generated from file.sh
# file.out the concatenation of file1.tmp and file2.tmp

all: file.out

file.out : file1.tmp file2.tmp
	cat $? > $@

%1.tmp %2.tmp: %.sh
	ksh ./$^

clean:
	rm -f file.out file1.tmp file2.tmp

# ----- test -----
.PHONY: test

# check if file.out is created
# check if file1.tmp and file2.tmp is created
# check if file.out contains "This is a test file."
# if so echo "[OK]" else echo "[KO]"
test:
	@if [ -f file.out ] && [ -f file1.tmp ] && [ -f file2.tmp ]; then \
		cat file.out | grep "This is a test file." > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
