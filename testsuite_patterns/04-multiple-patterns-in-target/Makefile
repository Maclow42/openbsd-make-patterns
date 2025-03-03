# Goal: observe behavior of %-%.in
# Subtility: the second % is useless
#
# Explanation: Use two % in target do not work (tested) and use a %-file.in instead of file-%.in do not work nor with file-file.in

all: file.out

%.out : %-%.in
	cp $? $@

clean:
	rm -f file.out

# ----- test -----
PHONY: test

# check if file.out is created
# check if it contains "This is a test file."
# if so echo "[OK]" else echo "[KO]"
test: all
	@cat file.out | grep "This is a test file." > /dev/null && echo "[OK]" || echo "[KO]"
