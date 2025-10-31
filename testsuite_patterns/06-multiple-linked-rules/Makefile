# Goal: Create a file in two steps
# file.tmp is explicitly asked so it is not deleted at the end
# file.in -> file.tmp -> file.out

all: file.out file.tmp

%.out: %.tmp
	cp $*.tmp $@
	echo " file." >> $@

%.tmp: %.in
	cat $*.in | tr '\n' ' ' > $@
	echo -n "a test" >> $@

clean:
	rm -f file.out file.tmp

# ----- test -----
.PHONY: test

test:
	@if [ -f file.out ] && [ -f file.tmp ]; then \
		if grep -q "This is a test file." file.out 2>/dev/null; then \
			echo "[OK]"; \
		else \
			echo "[KO]"; \
		fi; \
	else \
		echo "[KO]"; \
	fi
