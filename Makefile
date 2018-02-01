.PHONY: all
all: check

clean:
	find . -name "*.pyc" -type f -delete

check:
	PYTHONPATH="./rplugin/python3:$$PYTHONPATH" nosetests
