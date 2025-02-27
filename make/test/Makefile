# Goal: Create a file.out file from a file.in file
# Subtility: dependency is implicitly defined

all: file.out

%out : file.in
	cp file.in file.out

clean:
	rm -f file.out


# ----- test -----
PHONY: test

# check if file.out is created
# check if it contains "This is a test file."
# if so echo "[OK]" else echo "[KO]"
test: all
	@cat file.out | grep "This is a test file." > /dev/null && echo "[OK]" || echo "[KO]"
