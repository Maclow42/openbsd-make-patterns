# Goal: Test nested/chained pattern rules (multi-stage transformation)
# Subtility: .in -> .tmp -> .mid -> .out (3 transformations)

all: file.out

%.out: %.mid
	cp $*.mid $@
	echo " [stage3]" >> $@

%.mid: %.tmp
	cp $*.tmp $@
	echo " [stage2]" >> $@

%.tmp: %.in
	cp $*.in $@
	echo " [stage1]" >> $@

clean:
	rm -f file.out file.mid file.tmp

# ----- test -----
.PHONY: test

# check if file.out is created
# check if it contains all three stages
test:
	@if [ -f file.out ]; then \
		cat file.out | grep "This is a test file." > /dev/null && \
		cat file.out | grep "\[stage1\]" > /dev/null && \
		cat file.out | grep "\[stage2\]" > /dev/null && \
		cat file.out | grep "\[stage3\]" > /dev/null && \
		echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
