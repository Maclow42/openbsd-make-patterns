# Goal: use a single implicit rule to generate two dependencies
# Same as before but file1.tmp and file2.tmp are implicitly declared and deleted at the end

all: file.out

%.out : %1.tmp %2.tmp
	cat $? > $@

%1.tmp %2.tmp: %.sh
	ksh ./$<

clean:
	rm -f file.out

# ----- test -----
PHONY: test

# check if file.out is created
# check if file1.tmp and file2.tmp has been deleted
# check if file.out contains "This is a test file."
# if so echo "[OK]" else echo "[KO]"
test:
	@if [ -f file.out ] && [ ! -f file1.tmp ] && [ ! -f file2.tmp ]; then \
		cat file.out | grep "This is a test file." > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
