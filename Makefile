PYDIR=python/
TESTS=BufstackTests

.PHONY: all
all: check

clean:
	rm -r -f $(PYDIR)/*.pyc

check:
	cd $(PYDIR) && python -m unittest $(TESTS)

.PHONY: tags
tags:
	ctags -R --exclude=bin --c++-kinds=+p --fields=+iaS --extras=+q --language-force=C++ .
