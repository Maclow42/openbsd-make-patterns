# Goal: Test patterns with slash in the stem
# Subtility: % can match paths with slashes

all: out/subdir/file.result

out/%.result: src/%.data
	mkdir -p out/subdir
	cp src/subdir/file.data out/subdir/file.result

clean:
	rm -rf out

# ----- test -----
.PHONY: test

# check if out/subdir/file.result is created
test:
	@if [ -f out/subdir/file.result ]; then \
		cat out/subdir/file.result | grep "Data in subdirectory" > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
