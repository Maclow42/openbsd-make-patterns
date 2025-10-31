# Goal: Test pattern rule with empty recipe (semicolon only)
# Subtility: Empty recipe tells make the target doesn't need to be rebuilt

all: file.out

# Pattern rule with empty recipe
%.out: %.in ;

# Explicit rule that will be used
file.out: file.in
	echo "Explicit rule used" > file.out

clean:
	rm -f file.out

# ----- test -----
.PHONY: test

# check if file.out is created
# check if explicit rule was used
test:
	@if [ -f file.out ]; then \
		cat file.out | grep "Explicit rule used" > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
