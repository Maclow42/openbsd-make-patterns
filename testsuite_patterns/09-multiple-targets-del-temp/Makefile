# Goal: use a single implicit rule to generate two dependencies
# file1.tmp and file2.tmp are implicitly declared and deleted at the end

all: file.out

%.out: %1.tmp %2.tmp
	cat $*1.tmp $*2.tmp > $@

%1.tmp %2.tmp: %.sh
	ksh ./$*.sh

clean:
	rm -f file.out

# ----- test -----
.PHONY: test

test:
	@if [ -f file.out ] && [ ! -f file1.tmp ] && [ ! -f file2.tmp ]; then \
		if grep -q "This is a test file." file.out 2>/dev/null; then \
			echo "[OK]"; \
		else \
			echo "[KO]"; \
		fi; \
	else \
		echo "[KO]"; \
	fi
