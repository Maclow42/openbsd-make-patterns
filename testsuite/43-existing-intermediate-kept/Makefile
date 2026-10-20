# 43-existing-intermediate-kept
# Test that an intermediate file which exists before the run, and is up
# to date, is neither rebuilt nor removed: only the intermediate files
# make creates are removed

# ========== TEST RULES ==========
all:
	@echo "Source." > file.in
	@sleep 1
	@echo "Test passed." > file.tmp
	@$(MAKE) -s build > /dev/null 2>&1

build: file.out

%.out: %.tmp
	@cp $< $@
	@echo $@ >> builds

%.tmp: %.in
	@echo "Test failed." > $@
	@echo $@ >> builds

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && grep -q "Test passed." file.out && \
	    [ -f file.tmp ] && grep -q "Test passed." file.tmp && \
	    [ "`cat builds | tr '\n' ' '`" = "file.out " ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
