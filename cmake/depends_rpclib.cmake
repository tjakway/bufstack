if(USE_SYSTEM_RPCLIB)
    #note: system rpclib is untested
    find_package(rpc 2.2.1 REQUIRED)
else()
    #set rpclib options
    set(RPCLIB_CXX_STANDARD 11 CACHE STRING "Require C++11" FORCE)
    set(RPCLIB_ENABLE_LOGGING ON CACHE BOOL "Enable rpclib logging" FORCE)

    add_subdirectory(${CMAKE_SOURCE_DIR}/lib/rpclib 
        ${CMAKE_BINARY_DIR}/rpclib
        EXCLUDE_FROM_ALL)
endif()

function(depends_rpclib TARGET_NAME_PARAM)
    if(USE_SYSTEM_RPCLIB)
        target_include_directories(${TARGET_NAME_PARAM} SYSTEM INTERFACE ${RPCLIB_INCLUDE_DIRS})
        target_link_libraries(${TARGET_NAME_PARAM} INTERFACE ${RPCLIB_LIBRARIES})
    else()
       #target_include_directories(${TARGET_NAME_PARAM} SYSTEM PUBLIC
       #    ${CMAKE_SOURCE_DIR}/lib/rpclib/include)

        #unfortunately there is no namespace support yet for target names in nested projects,
        #only in install
        #see https://gitlab.kitware.com/cmake/cmake/issues/16414
        #so when using FIND_PROJECT(..) you would call target_link_libraries(... rpclib::rpc)
        #but when adding a subdirectory all the targets go into the global namespace so its
        #just rpc
        target_link_libraries(${TARGET_NAME_PARAM} PRIVATE rpc)
        target_compile_definitions(${TARGET_NAME_PARAM} 
            PUBLIC RPCLIB_MSGPACK=msgpack RPCLIB_FMT=fmt)
    endif()
endfunction()
