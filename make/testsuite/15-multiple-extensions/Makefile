# 15-multiple-extensions
# Test pattern rules with multiple extensions (make chooses based on existence)

# ========== TEST RULES ==========
all: file.o

%.o: %.c
	@echo "Compiled from: file.c" > file.o

%.o: %.cpp
	@echo "Compiled from: file.cpp" > file.o

# ========== INPUT FILES ==========
file.c:
	@echo "/* C source */" > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.o ] && grep -q "Compiled from: file.c" file.o; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f `ls | grep -v Makefile`
