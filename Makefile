MKFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
MKFILE_DIR := $(dir $(MKFILE_PATH))

CMAKE_SRC_DIR=$(MKFILE_DIR)
BIN_DIR=$(MKFILE_DIR)/bin

#use clang/clang++ if they exist, otherwise use gcc
DEFAULT_CMAKE_COMPILERS=$(shell command -v clang 1> /dev/null 2>&1 \
			    && echo -n "-DCMAKE_C_COMPILER=clang " \
				|| echo -n "-DCMAKE_C_COMPILER=gcc " ; \
			    command -v clang++ 1> /dev/null 2>&1 \
				&& echo -n "-DCMAKE_CXX_COMPILER=clang++ " \
				|| echo -n "-DCMAKE_C_COMPILER=g++ " )

#if lldb is installed and we're building with clang then use it
#otherwise use gdb
DEBUGGER=$(shell echo "$(DEFAULT_CMAKE_COMPILERS)" | \
	 grep -q "clang" && ( command -v lldb || echo "gdb" ) \
	 || echo "gdb")

TEST_EXE_NAME=bufstack_test

EXPORT_COMPILE_COMMANDS=-DCMAKE_EXPORT_COMPILE_COMMANDS=ON

DEFAULT_CMAKE_BUILD_TARGET=Debug
DEFAULT_CMAKE_ARGS=$(DEFAULT_CMAKE_COMPILERS) -DCMAKE_BUILD_TYPE=$(DEFAULT_CMAKE_BUILD_TARGET) $(EXPORT_COMPILE_COMMANDS) -DBUILD_TESTS=ON

PYDIR=python/
TESTS=BufstackTests

#the snippet `printf '%s' "$$x" | grep -v -q '^\s*$$'`
#tests if a variable is whitespace
GTEST_FILTER?=*
#if the variable GTEST_FILTER is set, construct the appropriate
#argument
GTEST_ARGS='--gtest_filter=$(GTEST_FILTER)'

#if we're passing gtest args, format them properly for
#the debugger
#note: need to cd to the test dir before calling the debugger
DEBUGGER_ARGS=-- './$(TEST_EXE_NAME)' $(GTEST_ARGS) 

.PHONY: all
all: $(BIN_DIR)/Makefile $(BIN_DIR)/compile_commands.json build

clean:
	rm -r -f $(BIN_DIR)

#exclude the compilation database from tag generation
.PHONY: tags
tags:
	ctags -R --exclude=bin \
	    --exclude=compile_commands.json \
	    --file-scope=yes \
	    --langmap=c:+.h \
	    --links=yes \
	    --c-kinds=+p \
	    -I __attribute__,__attribute_deprecated__,__attribute_format_arg__,__attribute_format_strfmon__,__attribute_malloc__,__attribute_noinline__,__attribute_pure__,__attribute_used__,__attribute_warn_unused_result__,__wur,__THROW,__nonnull+ \
	    --c++-kinds=+p --fields=+liaS --extras=+q --language-force=C++ .




#@ = don't echo line
.PHONY: check_cmake_src_dir
check_cmake_src_dir:
	@[ -f "$(CMAKE_SRC_DIR)/CMakeLists.txt" ] \
	    || ( echo "Could not find CMakeLists.txt in cmake source dir $(CMAKE_SRC_DIR)" ; \
		exit 1)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

.PHONY: cmake_gen
cmake_gen: $(BIN_DIR) check_cmake_src_dir
	cd $(BIN_DIR) && cmake $(DEFAULT_CMAKE_ARGS) $(CMAKE_SRC_DIR)

$(BIN_DIR)/Makefile: cmake_gen

#need for YouCompleteMe
$(BIN_DIR)/compile_commands.json: cmake_gen


.PHONY: build
build: cmake_gen
	cmake --build $(BIN_DIR)

#you can pass arguments to google test via environment variables
.PHONY: check
check: build
	GTEST_FILTER=$(GTEST_FILTER) cmake --build $(BIN_DIR) --target check

.PHONY: debug
debug: build
	cd $(BIN_DIR)/test/src && $(DEBUGGER) $(DEBUGGER_ARGS)
