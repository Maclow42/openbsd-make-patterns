# 09-multiple-targets-del-temp
# Test multiple targets with intermediate file deletion

# ========== TEST RULES ==========
all: file.out

%.out: %1.tmp %2.tmp
	@cat file1.tmp file2.tmp > file.out

%1.tmp %2.tmp: %.in
	@echo "temp" > file1.tmp
	@echo "temp" > file2.tmp

# ========== INPUT FILES ==========
file.in:
	@echo "input" > $@

# ========== VALIDATION RULES ==========
.PHONY: test

test:
	@if [ -f file.out ] && [ ! -f file1.tmp ] && [ ! -f file2.tmp ]; then \
		if grep -q "temp" file.out 2>/dev/null; then \
			echo "[OK]"; \
		else \
			echo "[KO]"; \
		fi; \
	else \
		echo "[KO]"; \
	fi

clean:
	@rm -f `ls | grep -v Makefile`
