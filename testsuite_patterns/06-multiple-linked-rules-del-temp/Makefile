# Goal: Create a file in two steps
# file.tmp is IMplicitly asked so it is deleted at the end
# file.in -> file.tmp -> file.out
# file.in + "a test" = file.tmp
# file.tmp + " file." = file.out

all: file.out

%.out: %.tmp
	cat $< > $@
	echo -n " file." >> $@

%.tmp: %.in
	cat $< | tr '\n' ' ' > $@
	echo -n "a test" >> $@

clean:
	rm -f file.out file.tmp

# ----- test -----
PHONY: test

# check if file.out is created
# check if file.tmp has been deleted
# check if file.out contains "This is a test file."
# if so echo "[OK]" else echo "[KO]"
test: all
	@if [ -f file.out ] && [ ! -f file.tmp ]; then \
		cat file.out | grep "This is a test file." > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
