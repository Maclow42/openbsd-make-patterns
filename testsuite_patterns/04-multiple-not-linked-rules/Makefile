# Goal: generate two dependencies using the same rule

all: file.out

file.out: file1.out file2.out
	# concat file1.out and file2.out into file.out
	# add a space between the two files
	cat file1.out | tr '\n' ' ' > $@; cat file2.out >> $@ 

%.out: %.in
	cp $? $@

clean:
	rm -f file.out file1.out file2.out

# ----- test -----
PHONY: test

# check if file.out, file1.out and file2.out are created
# check if file.out contains "This is a test file."
# if so echo "[OK]" else echo "[KO]"
test: all
	@if [ -f file.out ] && [ -f file1.out ] && [ -f file2.out ]; then \
		cat file.out | grep "This is a test file." > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
