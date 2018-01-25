cmake_minimum_required(VERSION 3.1)
project(bufstack-test)

FILE(GLOB MAIN_TEST_SRCS *.cpp *.c *.cxx)

add_executable(${MAIN_TEST_TARGET} ${MAIN_TEST_SRCS})

target_link_libraries(${MAIN_TEST_TARGET} PRIVATE ${MAIN_LIB_TARGET})
