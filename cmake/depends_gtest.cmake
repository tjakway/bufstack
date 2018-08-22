#build google test
enable_testing()
add_subdirectory("${CMAKE_SOURCE_DIR}/lib/googletest" 
    ${CMAKE_BINARY_DIR}/gtest-bin
    EXCLUDE_FROM_ALL)

function(depends_gtest TARGET_NAME_PARAM)
    target_include_directories(${TARGET_NAME_PARAM} PUBLIC SYSTEM ${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})
    target_include_directories(${TARGET_NAME_PARAM} PUBLIC SYSTEM ${GTEST_INCLUDE_DIRS})

    target_link_libraries(${TARGET_NAME_PARAM} PUBLIC ${MAIN_LIB_TARGET})
    target_link_libraries(${TARGET_NAME_PARAM} PUBLIC gtest gtest_main)
endfunction()
