# Goal: Test pattern rule with empty stem
# Subtility: What happens when the stem ($*) is empty?

all: .out

%.out: %.in
	echo "Stem is: '$*'" > $@
	cat $*.in >> $@

clean:
	rm -f .out

# ----- test -----
.PHONY: test

# check if .out is created
# check if stem is empty and file contains the input
test:
	@if [ -f .out ]; then \
		cat .out | grep "Stem is: ''" > /dev/null && \
		cat .out | grep "Empty stem test" > /dev/null && \
		echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
