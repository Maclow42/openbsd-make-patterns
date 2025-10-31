# Goal: Test pattern rules with subdirectories in the pattern
# Subtility: The % should work correctly with directory paths

all: output/file.out

output/%.out: input/%.in
	mkdir -p output
	cp input/$*.in $@
	echo " - processed" >> $@

clean:
	rm -rf output

# ----- test -----
.PHONY: test

# check if output/file.out is created
# check if it contains "This is a test file."
test:
	@if [ -f output/file.out ]; then \
		cat output/file.out | grep "This is a test file." > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
