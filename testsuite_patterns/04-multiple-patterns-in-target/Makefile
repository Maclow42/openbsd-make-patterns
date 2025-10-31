# Goal: observe behavior of %-%.in
# Subtility: the second % is useless

all: file.out

%.out: %-%.in
	cp $*-%.in $@

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
