# Goal: Test pattern rules with multiple file extensions
# Test if make chooses the correct source based on what exists

all: program.o

# Pattern rule for C files
%.o: %.c
	echo "Compiled from C: program.c" > program.o

# Pattern rule for C++ files  
%.o: %.cpp
	echo "Compiled from C++: program.cpp" > program.o

clean:
	rm -f program.o

# ----- test -----
.PHONY: test

# check if program.o is created
# check if it was compiled from the .c file (since we provide program.c)
test:
	@if [ -f program.o ]; then \
		cat program.o | grep "Compiled from C: program.c" > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
