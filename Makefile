SRCS := $(shell find . -name "*.py" -type f)

.PHONY: all
all: check tags

.PHONY: clean
clean:
	find . -name "*.pyc" -type f -delete

.PHONY: check
check:
	PYTHONPATH="./rplugin/python3:$$PYTHONPATH" nosetests

tags: $(SRCS)
	ctags -R . || /bin/true
