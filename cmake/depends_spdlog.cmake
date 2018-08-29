#configure spdlog as a dependency
set(SPDLOG_BUILD_TESTING CACHE FORCE OFF)

set(SPDLOG_DIR ${CMAKE_SOURCE_DIR}/lib/spdlog)
add_subdirectory(${SPDLOG_DIR} 
    ${CMAKE_BINARY_DIR}/spdlog-bin
    EXCLUDE_FROM_ALL)

set(SPDLOG_INCLUDE_DIRECTORIES ${SPDLOG_DIR}/include)
set(SPDLOG_TARGET_NAME spdlog)


function(depends_spdlog TARGET_NAME_PARAM)
    target_link_libraries(${TARGET_NAME_PARAM} INTERFACE ${SPDLOG_TARGET_NAME})
    target_include_directories(${TARGET_NAME_PARAM} 
        SYSTEM PUBLIC ${SPDLOG_INCLUDE_DIRECTORIES})
endfunction()
