cmake_minimum_required(VERSION 3.1)
add_subdirectory(src)

target_include_directories(${TEST_TARGET} PRIVATE include)
