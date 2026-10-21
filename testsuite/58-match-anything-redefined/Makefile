# 58-match-anything-redefined
# Test that a redefined match-anything rule does not chain on its first
# definition: prog is made from prog.c by the second "%: %.c" rule, and
# prog.c, a plain file, is not made from prog.c.c. The build runs under
# a CPU time limit, so that an endless chain fails instead of hanging

# ========== TEST RULES ==========
all:
	@printf 'prog:\n\n%%: %%.c\n\t@echo "First rule" > $$@\n\n%%: %%.c\n\t@cat $$< > $$@\n' > build.mk
	@echo "Test passed." > prog.c
	@ulimit -t 5; $(MAKE) -s -f build.mk prog

# ========== VALIDATION RULES ==========
test:
	@if [ -f prog ] && grep -q "Test passed." prog; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
