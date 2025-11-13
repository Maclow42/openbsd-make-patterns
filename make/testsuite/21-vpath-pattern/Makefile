# 22-vpath-pattern
# Test VPATH with pattern rules

# ========== TEST RULES ==========
all: file.out

VPATH = src

%.out: %.in
	@echo "Test passed." > $@

# ========== INPUT FILES ==========
src/file.in:
	@mkdir -p src
	@echo "input" > $@

# ========== VALIDATION RULES ==========
test:
	@ok=0; \
	if [ -f file.out ] && grep -q "Test passed." file.out; then \
		ok=`expr $$ok + 1`; \
	fi; \
	if [ -f src/file.in ]; then \
		ok=`expr $$ok + 1`; \
	fi; \
	if [ $$ok -eq 2 ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v Makefile`
