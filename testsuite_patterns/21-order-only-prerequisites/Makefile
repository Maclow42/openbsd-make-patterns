# Goal: Test order-only prerequisites with pattern rules
# Subtility: Order-only prerequisites (|) are not checked for modification time

all: file.out

%.out: %.in | output-dir
	cp file.in file.out

output-dir:
	@echo "Directory created" > dir.stamp

clean:
	rm -f file.out dir.stamp

# ----- test -----
.PHONY: test

# check if file.out is created
# check if dir.stamp was created (order-only prerequisite was built)
test:
	@if [ -f file.out ] && [ -f dir.stamp ]; then \
		cat file.out | grep "This is a test file." > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
