# Goal: Test pattern rule without recipe (cancels implicit rules)
# Subtility: Pattern rule without recipe should prevent default behavior

all: file.out

# This pattern rule has no recipe, it should prevent any implicit rule
# from being used
%.out: %.in

# Explicit rule with recipe
file.out: file.in
	echo "Explicit rule used" > $@

clean:
	rm -f file.out

# ----- test -----
.PHONY: test

# check if file.out is created
# check if it contains "Explicit rule used"
test:
	@if [ -f file.out ]; then \
		cat file.out | grep "Explicit rule used" > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
