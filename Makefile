SRCS := $(shell find . -name "*.py" -type f)

.PHONY: all
all: check

.PHONY: clean
clean: clean-pyc clean-pycache

.PHONY: clean-pyc
clean-pyc:
	find . -name "*.pyc" -type f -delete

.PHONY: clean-pycache
clean-pycache:
	find . -name "__pycache__" -type d -exec rm -r -f {} +

.PHONY: check
check:
	PYTHONPATH="./rplugin/python3:$$PYTHONPATH" nosetests

tags: $(SRCS)
	ctags -R . || /bin/true
