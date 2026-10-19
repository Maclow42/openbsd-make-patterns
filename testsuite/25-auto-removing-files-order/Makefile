# 25-auto-removing-files-order
# Test of automatic file removal with pattern rules

# ========== VARIABLES ==========
MSG_COMPILATION := "Compilation succeed."
MSG_OUT_EXISTS := "file.out exists."
MSG_OUT_NOT_EXISTS := "file.out does not exist."
MSG_TMP_EXISTS := "file.tmp exists."
MSG_TMP_NOT_EXISTS := "file.tmp does not exist."

# ========== TEST RULES ==========
all: file.out test1 test2

%.out: %.tmp
	@cat $? > $@
%.tmp: %.in
	@cat $? > $@

test1: test2
	@if [ -f file.out ]; then \
		echo $(MSG_OUT_EXISTS) > $@; \
	else \
		echo $(MSG_OUT_NOT_EXISTS) > $@; \
	fi
test2:
	@if [ -f file.tmp ]; then \
		echo $(MSG_TMP_EXISTS) > $@; \
	else \
		echo $(MSG_TMP_NOT_EXISTS) > $@; \
	fi

# ========== INPUT FILES ==========
file.in:
	@echo $(MSG_COMPILATION) > $@

# ========== VALIDATION RULES ==========
.PHONY: test

test:
	@ok=0; \
	if [ ! -f file.tmp ]; then \
		ok=`expr $$ok + 1`; \
	fi; \
	if [ -f file.out ] && grep -q $(MSG_COMPILATION) file.out; then \
		ok=`expr $$ok + 1`; \
	fi; \
	if [ -f test1 ] && grep -q $(MSG_OUT_EXISTS) test1; then \
		ok=`expr $$ok + 1`; \
	fi; \
	if [ -f test2 ] && grep -q $(MSG_TMP_EXISTS) test2; then \
		ok=`expr $$ok + 1`; \
	fi; \
	if [ $$ok -eq 4 ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -f `ls | grep -v Makefile`
