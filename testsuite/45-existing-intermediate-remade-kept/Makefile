# 45-existing-intermediate-remade-kept
# Test that an intermediate file which exists before the run, but is
# out of date, is remade and then kept: as in GNU make, only the
# intermediate files make creates are removed

# ========== TEST RULES ==========
all:
	@echo "Old." > file.tmp
	@sleep 1
	@echo "Test passed." > file.in
	@$(MAKE) -s build > /dev/null 2>&1

build: file.out

%.out: %.tmp
	@cp $< $@
	@echo $@ >> builds

%.tmp: %.in
	@cp $< $@
	@echo $@ >> builds

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.out ] && grep -q "Test passed." file.out && \
	    [ -f file.tmp ] && grep -q "Test passed." file.tmp && \
	    [ "`cat builds | tr '\n' ' '`" = "file.tmp file.out " ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
