cmake_minimum_required(VERSION 3.1)
project(bufstack-src)

FILE(GLOB MAIN_EXE_SRCS *.cpp *.c *.cxx)

add_executable(${MAIN_EXE_TARGET} ${MAIN_EXE_SRCS})
target_link_libraries(${MAIN_EXE_TARGET} PRIVATE ${MAIN_LIB_TARGET})

process_warnings(${MAIN_EXE_TARGET})
