cmake_minimum_required(VERSION 3.1)
project(bufstack-test)

FILE(GLOB MAIN_TEST_SRCS *.cpp *.c *.cxx)


add_executable(${TEST_TARGET} ${MAIN_TEST_SRCS})

target_link_libraries(${TEST_TARGET} PRIVATE ${MAIN_LIB_TARGET})

depends_gtest(${TEST_TARGET})
depends_msgpack(${TEST_TARGET})
depends_rpclib(${TEST_TARGET})


#test target synonyms
add_custom_target(check COMMAND ${TEST_TARGET} 
    DEPENDS ${TEST_TARGET} 
    WORKING_DIRECTORY ${TESTS_BIN_DIR})

add_custom_target(check-all COMMAND check)

process_warnings(${TEST_TARGET})

#copy the test resources dir to the same directory as the test executable
file(COPY ${TEST_RESOURCES_DIRECTORY} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
