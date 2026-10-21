# 54-pattern-rule-existing-prerequisite
# Test that, as in GNU make, a pattern rule whose prerequisite cannot be
# found is skipped for the next one: %.o: %.cpp comes first, but only
# file.c exists, so %.o: %.c is chosen

# ========== TEST RULES ==========
all: file.o

%.o: %.cpp
	@echo "Compiled from: $<" > $@

%.o: %.c
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
