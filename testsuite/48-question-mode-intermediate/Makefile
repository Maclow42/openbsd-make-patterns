# 48-question-mode-intermediate
# Test make -q once the intermediate file is removed, without -j and
# with -j4: the target is reported up to date (status 0), and out of
# date (non-zero status) once its source changes, without running any
# command

# ========== TEST RULES ==========
all:
	@$(MAKE) -s file.out > /dev/null 2>&1
	@cp builds builds.first
	@$(MAKE) -s -q file.out > /dev/null 2>&1 && \
	    echo 0 > q.seq.first || echo 1 > q.seq.first
	@$(MAKE) -s -j4 -q file.out > /dev/null 2>&1 && \
	    echo 0 > q.j4.first || echo 1 > q.j4.first
	@sleep 1
	@touch file.in
	@$(MAKE) -s -q file.out > /dev/null 2>&1 && \
	    echo 0 > q.seq.second || echo 1 > q.seq.second
	@$(MAKE) -s -j4 -q file.out > /dev/null 2>&1 && \
	    echo 0 > q.j4.second || echo 1 > q.j4.second

%.out: %.tmp
	@cp $< $@
	@echo $@ >> builds

%.tmp: %.in
	@cp $< $@
	@echo $@ >> builds

# ========== INPUT FILES ==========
file.in:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && grep -q "Test passed." file.out && \
	    [ ! -f file.tmp ] && cmp -s builds builds.first && \
	    [ "`cat q.seq.first`" -eq 0 ] && [ "`cat q.j4.first`" -eq 0 ] && \
	    [ "`cat q.seq.second`" -ne 0 ] && \
	    [ "`cat q.j4.second`" -ne 0 ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
