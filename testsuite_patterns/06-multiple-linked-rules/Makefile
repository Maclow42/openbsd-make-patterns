# Goal: Create a file in two steps
# file.tmp is explicitly asked so it is not deleted at the end
# file.in -> file.tmp -> file.out

all: file.out file.tmp

%.out: %.tmp
	cp file.tmp file.out
	echo " file." >> file.out

%.tmp: %.in
	cat file.in | tr '\n' ' ' > file.tmp
	echo -n "a test" >> file.tmp

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
