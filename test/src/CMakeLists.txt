cmake_minimum_required(VERSION 3.1)
project(bufstack-test)

FILE(GLOB MAIN_TEST_SRCS *.cpp *.c *.cxx)

#build google test
enable_testing()
add_subdirectory("${CMAKE_SOURCE_DIR}/lib/googletest" ${CMAKE_BINARY_DIR}/gtest-bin)

add_executable(${TEST_TARGET} ${MAIN_TEST_SRCS})

target_link_libraries(${TEST_TARGET} PRIVATE ${MAIN_LIB_TARGET})


target_include_directories(${TEST_TARGET} PUBLIC SYSTEM ${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})
target_include_directories(${TEST_TARGET} PUBLIC SYSTEM ${GTEST_INCLUDE_DIRS})

target_link_libraries(${TEST_TARGET} PUBLIC ${MAIN_LIB_TARGET})
target_link_libraries(${TEST_TARGET} PUBLIC gtest gtest_main)

#test target synonyms
add_custom_target(check COMMAND ${TEST_TARGET} 
    DEPENDS ${TEST_TARGET} 
    WORKING_DIRECTORY ${TESTS_BIN_DIR})

add_custom_target(check-all COMMAND check)

process_warnings(${TEST_TARGET})
