# Goal: use % as a directory name

all: file.out

%.out: ./src/tmp/%.tmp
	cat $> > $@

%.tmp:
	echo "This is a test file." > $@

clean:
	rm -rf file.out src/tmp/*.tmp

# ----- test -----
.PHONY: test

test:
	@if [ -f file.out ]; then \
		if grep -q "This is a test file." file.out 2>/dev/null; then \
			echo "[OK]"; \
		else \
			echo "[KO]"; \
		fi; \
	else \
		echo "[KO]"; \
	fi
