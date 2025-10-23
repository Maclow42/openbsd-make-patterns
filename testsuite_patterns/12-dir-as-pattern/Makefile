# Goal: use % as a directory name

all: file.out

%.out: ./src/tmp/%.tmp
	cat $? | tr '\n' ' ' > $@

%.tmp:
	echo -n "This is a test file." > $@

clean:
	rm -rf file.out

# ----- test -----
.PHONY: test

# check if file.out is created
# check if file.out contains "This is a test file."
# if so echo "[OK]" else echo "[KO]"
test:
	@if [ -f file.out ]; then \
		cat file.out | grep "This is a test file." > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
