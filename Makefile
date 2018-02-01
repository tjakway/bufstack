PYDIR=python/
TESTS=BufstackTests

.PHONY: all
all: check

clean:
	find . -name "*.pyc" -type f -delete

check:
	cd $(PYDIR) && python -m unittest $(TESTS)
