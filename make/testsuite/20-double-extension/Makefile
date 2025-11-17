# 20-double-extension
# Test pattern rules with double extensions like .tar.gz
# Subtility: Pattern should correctly handle multiple dots in filename

# ========== TEST RULES ==========
all: file.tar.gz

%.tar.gz: %.tar
	@cp $? $@
	@echo " compressed" >> $@

%.tar: %.in
	@cp $? $@
	@echo " archived" >> $@

# ========== INPUT FILES ==========
file.in:
	@echo "Test passed" > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f file.tar.gz ]; then \
		cat file.tar.gz | grep "Test passed" > /dev/null && \
		cat file.tar.gz | grep "archived" > /dev/null && \
		cat file.tar.gz | grep "compressed" > /dev/null && \
		echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi

clean:
	@rm -f `ls | grep -v Makefile`
