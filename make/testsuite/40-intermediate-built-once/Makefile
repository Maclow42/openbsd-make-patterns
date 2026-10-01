# 40-intermediate-built-once
# Test that each file of a chain of pattern rules is built once: the
# commands of an intermediate file run a single time

# ========== TEST RULES ==========
all: doc.html

%.html: %.tmp
	@cp $< $@
	@echo $@ >> builds

%.tmp: %.md
	@cp $< $@
	@echo $@ >> builds

# ========== INPUT FILES ==========
doc.md:
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f doc.html ] && grep -q "Test passed." doc.html && \
	    [ "`cat builds | tr '\n' ' '`" = "doc.tmp doc.html " ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
