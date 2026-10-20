# 49-intermediate-without-pattern-rule
# Test .INTERMEDIATE without any pattern rule: file.mid, made by its own
# rule, is removed after the build, and a second run does not make it
# again

# ========== TEST RULES ==========
all:
	@$(MAKE) -s build > /dev/null 2>&1
	@cp builds builds.first
	@$(MAKE) -s build > /dev/null 2>&1

build: file.out

.INTERMEDIATE: file.mid

file.out: file.mid
	@cat file.mid > $@
	@echo $@ >> builds

file.mid: file.in
	@cat file.in > $@
	@echo $@ >> builds

# ========== INPUT FILES ==========
file.in:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && grep -q "Test passed." file.out && \
	    [ ! -f file.mid ] && cmp -s builds.first builds && \
	    [ "`cat builds | tr '\n' ' '`" = "file.mid file.out " ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
