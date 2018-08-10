
#python virualenv rules
VENV_DIR=env
PYTHON_EXE=python3
ACTIVATE_SCRIPT=$(VENV_DIR)/bin/activate
REQUIREMENTS=requirements.txt

#note: dash doesn't have source
# . (the dot command) is the equivalent
#see https://unix.stackexchange.com/questions/58514/what-is-the-difference-between-and-source-in-shells
ACTIVATE=[ -f "$(ACTIVATE_SCRIPT)" ] && . "$(ACTIVATE_SCRIPT)"

SRCS := $(shell find . -name "*.py" -type f)

.PHONY: all
all: pip_install_reqs check

.PHONY: clean
clean: clean-pyc clean-pycache

.PHONY: clean-pyc
clean-pyc:
	find . -name "*.pyc" -type f -delete

.PHONY: clean-pycache
clean-pycache:
	find . -name "__pycache__" -type d -exec rm -r -f {} +

.PHONY: check
check: pip_install_reqs
	$(ACTIVATE) && PYTHONPATH="./rplugin/python3:$$PYTHONPATH" nosetests -v

tags: $(SRCS)
	ctags -R . || /bin/true


.PHONY: check-virtualenv-installed
check-virtualenv-installed:
	( $(PYTHON_EXE) -m virtualenv --version 1> /dev/null 2>&1 ) \
	    || ( echo "python module virtualenv for $(PYTHON_EXE) is not installed" ; \
		exit 1 )

#create the virtualenv
$(ACTIVATE_SCRIPT): check-virtualenv-installed
	[ ! -d "$(VENV_DIR)" ] \
	    && $(PYTHON_EXE) -m virtualenv -p $(PYTHON_EXE) $(VENV_DIR) \
	    || /bin/true

$(REQUIREMENTS): $(ACTIVATE_SCRIPT)
	[ -f "$(REQUIREMENTS)" ] || \
	    ( echo "$(REQUIREMENTS) does not exist," \
		" please add one enumerating dependencies" ; \
		exit 1 )

.PHONY: pip_install_reqs
pip_install_reqs: $(ACTIVATE_SCRIPT) $(REQUIREMENTS)
	$(ACTIVATE) && pip install -q -r $(REQUIREMENTS)
