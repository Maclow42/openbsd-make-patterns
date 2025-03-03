# Goal: Create a file.out file from a file.in file
# Subtility: veridy that a corresponding non-pattern rule is preferred over a pattern rule

all: file.out

%.out : file.in

file.out : file.in
	cat $? > $@

%.out : file.in

clean:
	rm -f file.out

# ----- test -----
PHONY: test

# check if file.out is created
# check if it contains "This is a test file."
# if so echo "[OK]" else echo "[KO]"
test: all
	@cat file.out | grep "This is a test file." > /dev/null && echo "[OK]" || echo "[KO]"
