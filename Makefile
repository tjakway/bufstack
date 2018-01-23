PYDIR=python/
TESTS=BufstackTests

.PHONY: all
all: check

clean:
	rm -r -f $(PYDIR)/*.pyc

check:
	cd $(PYDIR) && python -m unittest $(TESTS)
