# 36-pattern-parallel-build
# Test chained pattern rules built in parallel (-j4): every target is
# built and every intermediate file is removed

# ========== TEST RULES ==========
FILES = f1 f2 f3 f4 f5 f6

all:
	@$(MAKE) -s -j4 build

build: $(FILES:=.out)

%.out: %.tmp
	@cp $< $@

%.tmp: %.in
	@cp $< $@

# ========== INPUT FILES ==========
$(FILES:=.in):
	@echo "Test passed." > $@

# ========== VALIDATION RULES ==========
test:
	@ok=0; \
	for f in $(FILES); do \
		if [ -f $$f.out ] && grep -q "Test passed." $$f.out && \
		    [ ! -f $$f.tmp ]; then \
			ok=`expr $$ok + 1`; \
		fi; \
	done; \
	if [ $$ok -eq 6 ]; then \
		echo "[OK]"; \
		exit 0; \
	fi; \
	echo "[KO]"; \
	exit 1

clean:
	@rm -rf `ls | grep -v -e Makefile -e KNOWN_FAILURE`
