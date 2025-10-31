# Goal: Test static pattern rules (different from implicit pattern rules)
# Subtility: Static pattern rules: $(targets): target-pattern: prereq-patterns

OBJS = file1.o file2.o file3.o

all: $(OBJS)

# Static pattern rule
$(OBJS): %.o: %.c
	echo "Compiling $*.c to $@" > $@

clean:
	rm -f $(OBJS)

# ----- test -----
.PHONY: test

# check if all .o files are created
# check if they contain the expected compilation message
test:
	@if [ -f file1.o ] && [ -f file2.o ] && [ -f file3.o ]; then \
		cat file1.o | grep "Compiling file1.c" > /dev/null && \
		cat file2.o | grep "Compiling file2.c" > /dev/null && \
		cat file3.o | grep "Compiling file3.c" > /dev/null && \
		echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
