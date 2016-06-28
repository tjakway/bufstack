PYDIR=python/
TESTS=BufstackTests

clean:
	rm -r -f $(PYDIR)/*.pyc

check:
	cd $(PYDIR) && python -m unittest $(TESTS)
