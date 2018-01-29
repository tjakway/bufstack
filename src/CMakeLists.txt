cmake_minimum_required(VERSION 3.1)
project(bufstack-src)

FILE(GLOB MAIN_LIB_SRCS *.cpp *.c *.cxx)

add_library(${MAIN_LIB_TARGET} ${MAIN_LIB_SRCS})
target_include_directories(${MAIN_LIB_TARGET} SYSTEM PUBLIC ${CMAKE_SOURCE_DIR}/include)

depends_msgpack(${MAIN_LIB_TARGET})
process_warnings(${MAIN_LIB_TARGET})

add_subdirectory(exe)
