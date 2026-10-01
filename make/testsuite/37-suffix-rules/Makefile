# 37-suffix-rules
# Test that suffix rules keep working alongside pattern rules

# ========== TEST RULES ==========
.SUFFIXES: .src .res

all: foo.res bar.out

.src.res:
	@echo "suffix $<" > $@

%.out: %.in
	@echo "pattern $<" > $@

# ========== INPUT FILES ==========
foo.src bar.in:
	@touch $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f foo.res ] && grep -qx "suffix foo.src" foo.res && \
	    [ -f bar.out ] && grep -qx "pattern bar.in" bar.out; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
