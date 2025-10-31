# Goal: Create a file.out file from a file.in file
# Subtility: dependency is explicitly defined

all: file.out

%.out: file.in
	cp file.in $@

clean:
	rm -f file.out

# ----- test -----
.PHONY: test

test:
	@if grep -q "This is a test file." file.out 2>/dev/null; then \
		echo "[OK]"; \
	else \
		echo "[KO]"; \
	fi
