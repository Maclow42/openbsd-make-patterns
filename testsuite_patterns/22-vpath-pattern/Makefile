# Goal: Test pattern rules with vpath
# Subtility: vpath tells make where to search for prerequisite files

all: file.out

# Search for .in files in the src directory
vpath %.in src

%.out: %.in
	cp src/$*.in $@

clean:
	rm -f file.out

# ----- test -----
.PHONY: test

# check if file.out is created (source was found in src/ via vpath)
test:
	@if [ -f file.out ]; then \
		cat file.out | grep "This is a test file." > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
