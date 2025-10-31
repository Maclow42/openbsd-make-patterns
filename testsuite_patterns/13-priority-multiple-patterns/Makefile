# Goal: Test priority when multiple pattern rules match the same target
# Subtility: The first matching pattern rule should be used

# With GNU make, the last matching pattern rule is used.
# With BSD make, the first matching pattern rule is used.

all: file.out

# First pattern rule - should be used
%.out:
	echo "First rule" > $@

# Second pattern rule - should be ignored
%.out:
	echo "Second rule" > $@

clean:
	rm -f file.out

# ----- test -----
.PHONY: test

# check if file.out is created
# check if it contains "First rule" (not "Second rule")
# if so echo "[OK]" else echo "[KO]"
test:
	@if [ -f file.out ]; then \
		cat file.out | grep "First rule" > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
