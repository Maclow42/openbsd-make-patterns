# Goal: Test automatic variables in pattern rules
# Test $*, $@, $<, $^, $(@D), $(@F), $(<D), $(<F)

all: output/test.out

output/test.out: input/test.in input/extra.in
	@echo "Target: $@" > $@
	@echo "Stem: $*" >> $@
	@echo "First prereq: input/test.in" >> $@
	@echo "All prereqs: input/test.in input/extra.in" >> $@
	@echo "Target dir: output" >> $@
	@echo "Target file: test.out" >> $@
	@echo "First prereq dir: input" >> $@
	@echo "First prereq file: test.in" >> $@

output/%.out: input/%.in input/extra.in
	mkdir -p output
	@echo "Target: $@" > $@
	@echo "Stem: $*" >> $@
	@echo "First prereq: input/$*.in" >> $@
	@echo "All prereqs: input/$*.in input/extra.in" >> $@
	@echo "Target dir: output" >> $@
	@echo "Target file: $*.out" >> $@
	@echo "First prereq dir: input" >> $@
	@echo "First prereq file: $*.in" >> $@

clean:
	rm -rf output

# ----- test -----
.PHONY: test

# check if output/test.out is created and contains expected values
test:
	@if [ -f output/test.out ]; then \
		grep "Stem: test" output/test.out > /dev/null && \
		grep "Target: output/test.out" output/test.out > /dev/null && \
		grep "First prereq: input/test.in" output/test.out > /dev/null && \
		echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
