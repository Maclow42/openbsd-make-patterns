# Goal: Test if pattern rules work with .PHONY targets
# Subtility: Testing edge case of phony pattern rules

all: test-file

# This is NOT standard - phony pattern rules are unusual
# But we test the behavior anyway
.PHONY: test-%

test-%:
	@echo "Running test: $*" > $@.result

clean:
	rm -f test-file.result

# ----- test -----
.PHONY: test

# check if test-file.result was created
test:
	@if [ -f test-file.result ]; then \
		cat test-file.result | grep "Running test: file" > /dev/null && echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
