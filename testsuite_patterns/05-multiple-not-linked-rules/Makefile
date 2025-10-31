# Goal: generate two dependencies using the same rule

all: file.out

file.out: file1.out file2.out
	cat file1.out > $@
	echo -n " " >> $@
	cat file2.out >> $@

%.out: %.in
	cp $> $@

clean:
	rm -f file.out file1.out file2.out

# ----- test -----
.PHONY: test

test:
	@if [ -f file.out ] && [ -f file1.out ] && [ -f file2.out ]; then \
		if grep -q "This is a test file." file.out 2>/dev/null; then \
			echo "[OK]"; \
		else \
			echo "[KO]"; \
		fi; \
	else \
		echo "[KO]"; \
	fi
