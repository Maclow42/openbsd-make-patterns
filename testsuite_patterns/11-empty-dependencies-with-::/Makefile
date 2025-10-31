# Goal: tricky generation of a file.out file without any dependency
all:

file.out:

%::
	echo "This is a test file." > $@

clean:
	rm -f file.out clean test

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
