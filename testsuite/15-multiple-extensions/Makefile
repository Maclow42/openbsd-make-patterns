# 15-multiple-extensions
# Test pattern rules with multiple extensions (make chooses based on existence)

# ========== TEST RULES ==========
all: file.o

%.o: %.c
	@echo "Compiled from: $<" > $@

%.o: %.cpp
	@echo "Compiled from: $<" > $@

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
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
