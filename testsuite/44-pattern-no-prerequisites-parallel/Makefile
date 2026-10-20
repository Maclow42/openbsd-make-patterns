# 44-pattern-no-prerequisites-parallel
# Test that the commands of a pattern rule without prerequisites run a
# single time, in parallel mode (-j1 and -j4) as without -j

# ========== TEST RULES ==========
all:
	@$(MAKE) -s build > /dev/null 2>&1
	@mv builds builds.seq
	@rm file.out
	@$(MAKE) -s -j1 build > /dev/null 2>&1
	@mv builds builds.j1
	@rm file.out
	@$(MAKE) -s -j4 build > /dev/null 2>&1
	@mv builds builds.j4

build: file.out

%.out: %.tmp
	@cp $< $@
	@echo $@ >> builds

%.tmp:
	@echo "Test passed." > $@
	@echo $@ >> builds

# ========== VALIDATION RULES ==========
test:
	@ok=0; \
	if [ -f file.out ] && grep -q "Test passed." file.out && \
	    [ ! -f file.tmp ]; then \
		ok=`expr $$ok + 1`; \
	fi; \
	for f in builds.seq builds.j1 builds.j4; do \
		if [ "`cat $$f | tr '\n' ' '`" = "file.tmp file.out " ]; then \
			ok=`expr $$ok + 1`; \
		fi; \
	done; \
	if [ $$ok -eq 4 ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
