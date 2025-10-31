# Goal: Create a file.out file from a file.in file
# Subtility: verify that a corresponding non-pattern rule is preferred over a pattern rule

all: file.out

%.out: file.in

file.out: file.in
	cat file.in > $@

%.out: file.in

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
