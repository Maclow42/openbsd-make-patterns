# Goal: Test pattern rules with double extensions like .tar.gz
# Subtility: Pattern should correctly handle multiple dots in filename

all: archive.tar.gz

%.tar.gz: %.tar
	cp archive.tar archive.tar.gz
	echo " compressed" >> archive.tar.gz

%.tar: %.txt
	cp archive.txt archive.tar
	echo " archived" >> archive.tar

clean:
	rm -f archive.tar.gz archive.tar

# ----- test -----
.PHONY: test

# check if archive.tar.gz is created
# check if it contains the expected content
test:
	@if [ -f archive.tar.gz ]; then \
		cat archive.tar.gz | grep "This is a test file." > /dev/null && \
		cat archive.tar.gz | grep "archived" > /dev/null && \
		cat archive.tar.gz | grep "compressed" > /dev/null && \
		echo "[OK]" || echo "[KO]"; \
	else \
		echo "[KO]"; \
	fi
