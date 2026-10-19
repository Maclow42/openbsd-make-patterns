# 35-pattern-stem-with-directory
# Test targets in a directory: a pattern without directory keeps the
# directory in the stem (src/foo.out from src/foo.in), while a pattern
# with directories only matches the file part (build/bar.res from
# src/bar.in)

# ========== TEST RULES ==========
all: src/foo.out build/bar.res

%.out: %.in
	@echo "lt=$<" > $@

build/%.res: src/%.in
	@mkdir -p build
	@echo "lt=$<" > $@

# ========== INPUT FILES ==========
src/foo.in src/bar.in:
	@mkdir -p src
	@touch $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f src/foo.out ] && [ -f build/bar.res ] && \
	    grep -qx "lt=src/foo.in" src/foo.out && \
	    grep -qx "lt=src/bar.in" build/bar.res; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
