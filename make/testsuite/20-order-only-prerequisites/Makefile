# 21-order-only-prerequisites
# Test order-only prerequisites in pattern rules

# ========== TEST RULES ==========
all: file.out

%.out: %.in | dir_marker
	@echo "Test passed." > $@

%.in:
	@echo "input" > $@

dir_marker:
	@mkdir -p build
	@touch dir_marker

# ========== VALIDATION RULES ==========
test:
	@ok=0; \
	if [ -f file.out ] && grep -q "Test passed." file.out; then \
		ok=`expr $$ok + 1`; \
	fi; \
	if [ -f dir_marker ]; then \
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
