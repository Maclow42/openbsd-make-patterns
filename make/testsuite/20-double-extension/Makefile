# 20-double-extension
# Test pattern rules with double extensions like .tar.gz
# Subtility: Pattern should correctly handle multiple dots in filename

# ========== TEST RULES ==========
all: file.tar.gz

%.tar.gz: %.tar
	@cp $< $@
	@echo " compressed" >> $@

%.tar: %.in
	@cp $< $@
	@echo " archived" >> $@

# ========== INPUT FILES ==========
file.in:
	@echo "Test passed" > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.tar.gz ] && grep -q "Test passed" file.tar.gz && \
	    grep -q "archived" file.tar.gz && \
	    grep -q "compressed" file.tar.gz; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
