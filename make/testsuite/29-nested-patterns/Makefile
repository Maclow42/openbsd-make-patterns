# 30-nested-patterns
# Test nested pattern matching (3-stage transformation chain)

# ========== TEST RULES ==========
all: out/file.out

out/%.out: tmp/%.mid
	@mkdir -p out
	@cp $< $@
	@echo " [stage3]" >> $@

tmp/%.mid: tmp/%.tmp
	@mkdir -p tmp
	@cp $< $@
	@echo " [stage2]" >> $@

tmp/%.tmp: src/%.in
	@mkdir -p tmp
	@cp $< $@
	@echo " [stage1]" >> $@

# ========== INPUT FILES ==========
src/file.in:
	@mkdir -p src
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@if [ -f out/file.out ]; then \
		grep -q "Test passed." out/file.out && \
		grep -q "\[stage1\]" out/file.out && \
		grep -q "\[stage2\]" out/file.out && \
		grep -q "\[stage3\]" out/file.out && \
		echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi

clean:
	@rm -rf `ls | grep -v Makefile`

