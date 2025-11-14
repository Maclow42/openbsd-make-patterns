# 18-secondary-target
# Test .SECONDARY special target to preserve intermediate files
# SECONDARY doest not understand pattern rules

# ========== TEST RULES ==========
all: file.out

.SECONDARY: file.tmp

%.out: %.tmp
	@cat $? > $@

%.tmp: %.in
	@cat $? > $@

# ========== INPUT FILES ==========
file.in:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@ok=0; \
	if [ -f file.out ] && grep -q "Test passed." file.out; then \
		ok=`expr $$ok + 1`; \
	fi; \
	if [ -f file.tmp ] && grep -q "Test passed." file.tmp; then \
		ok=`expr $$ok + 1`; \
	fi; \
	if [ $$ok -eq 2 ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f `ls | grep -v Makefile`
