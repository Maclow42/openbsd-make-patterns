# Goal: Test nested/chained pattern rules (multi-stage transformation)
# Subtility: .in -> .tmp -> .mid -> .out (3 transformations)

all: file.out

%.out: %.mid
	cp file.mid file.out
	echo " [stage3]" >> file.out

%.mid: %.tmp
	cp file.tmp file.mid
	echo " [stage2]" >> file.mid

%.tmp: %.in
	cp file.in file.tmp
	echo " [stage1]" >> file.tmp

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
