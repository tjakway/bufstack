cmake_minimum_required(VERSION 3.1)

include(${CMAKE_SOURCE_DIR}/cmake/depends_gtest.cmake)

set(TEST_RESOURCES_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/resources")

add_subdirectory(src)

target_include_directories(${TEST_TARGET} PRIVATE include)
