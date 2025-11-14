# 14-patterned-file
# Searching a file matching a pattern rule
# Warning: file.in SHOULD NOT be deleted by being
# considered as temporary file

# ========== TEST RULES ==========
all: file.out

%.out: %.in
	@cat $? > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && [ -f file.in ]; then \
		if grep -q "Test passed." file.out; then \
			echo "[OK]"; \
			exit 0; \
		fi; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f `ls | grep -vE "(Makefile|file.in)"`
