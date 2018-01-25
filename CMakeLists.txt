cmake_minimum_required(VERSION 3.1)

project(bufstack)

set(MAIN_LIB_TARGET bufstack)
set(MAIN_EXE_TARGET bufstack_server)
set(TEST_TARGET bufstack_test)

#need to use our FindMsgpack.cmake script
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include("${CMAKE_SOURCE_DIR}/cmake/depends_msgpack.cmake")

set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 11)

OPTION(USE_SYSTEM_MSGPACK OFF)
OPTION(BUILD_TESTS ON)

add_subdirectory(src)

if(BUILD_TESTS)
    add_subdirectory(test)
endif()
