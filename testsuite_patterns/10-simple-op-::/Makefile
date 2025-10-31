# Goal: Simple test of % pattern with ::

all: file.out

%.out::
	touch $@

%.out::
	echo "This is a test file." > $@

clean:
	rm -f file.out

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
