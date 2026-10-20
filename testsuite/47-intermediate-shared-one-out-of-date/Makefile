# 47-intermediate-shared-one-out-of-date
# Test an intermediate file shared by two targets, without -j and with
# -j4: the first run makes it once for both, and once only one target is
# out of date, a second run makes it again for that target alone

# ========== TEST RULES ==========
all:
	@$(MAKE) -s build > /dev/null 2>&1
	@mv builds builds.seq.first
	@sleep 1
	@touch file.h
	@$(MAKE) -s build > /dev/null 2>&1
	@mv builds builds.seq.second
	@rm file.out file.log
	@$(MAKE) -s -j4 build > /dev/null 2>&1
	@mv builds builds.j4.first
	@sleep 1
	@touch file.h
	@$(MAKE) -s -j4 build > /dev/null 2>&1
	@mv builds builds.j4.second

build: file.out file.log

%.out: %.tmp
	@cp $< $@
	@echo $@ >> builds

%.log: %.tmp %.h
	@cp $< $@
	@echo $@ >> builds

%.tmp: %.in
	@sleep 1
	@cp $< $@
	@echo $@ >> builds

# ========== INPUT FILES ==========
file.in:
	@echo "Test passed." > $@

file.h:
	@echo "Header." > $@

# ========== VALIDATION RULES ==========
test:
	@ok=0; \
	if [ -f file.out ] && grep -q "Test passed." file.out && \
	    [ -f file.log ] && grep -q "Test passed." file.log && \
	    [ ! -f file.tmp ]; then \
		ok=`expr $$ok + 1`; \
	fi; \
	for m in seq j4; do \
		if [ "`sort builds.$$m.first | tr '\n' ' '`" = \
		    "file.log file.out file.tmp " ] && \
		    [ "`cat builds.$$m.second | tr '\n' ' '`" = \
		    "file.tmp file.log " ]; then \
			ok=`expr $$ok + 1`; \
		fi; \
	done; \
	if [ $$ok -eq 3 ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
