# Goal: Test pattern at different positions in target and prerequisite
# Subtility: lib%.a from %.o - % is at different position

all: libtest.a

lib%.a: %.o
	echo "Creating library $@ from $*.o" > $@

%.o: %.c
	echo "Compiling $*.c to $@" > $@

clean:
	rm -f libtest.a test.o

# ----- test -----
.PHONY: test

# check if libtest.a is created
# check if test.o was created as intermediate
test:
	@if [ -f libtest.a ]; then \
		cat libtest.a | grep "Creating library libtest.a from test.o" > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
