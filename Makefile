.PHONY: all
all: check

.PHONY: clean
clean:
	find . -name "*.pyc" -type f -delete

.PHONY: check
check:
	PYTHONPATH="./rplugin/python3:$$PYTHONPATH" nosetests

tags:
	ctags -R . || /bin/true
