function(depends_rpclib TARGET_NAME_PARAM)
    if(USE_SYSTEM_MSGPACK)
        #note: system rpclib is untested
        find_package(rpc 2.2.1 REQUIRED)

        target_include_directories(${TARGET_NAME_PARAM} SYSTEM INTERFACE ${MSGPACK_INCLUDE_DIRS})
        target_link_libraries(${TARGET_NAME_PARAM} INTERFACE ${MSGPACK_LIBRARIES})
    else()
        set(RPCLIB_CXX_STANDARD 11 CACHE BOOL "Require C++11" FORCE)

        add_subdirectory(${CMAKE_SOURCE_DIR}/lib/rpclib 
            ${CMAKE_BINARY_DIR}/rpclib
            EXCLUDE_FROM_ALL)

       #target_include_directories(${TARGET_NAME_PARAM} SYSTEM PUBLIC
       #    ${CMAKE_SOURCE_DIR}/lib/rpclib/include)

        #unfortunately there is no namespace support yet for target names in nested projects,
        #only in install
        #see https://gitlab.kitware.com/cmake/cmake/issues/16414
        #so when using FIND_PROJECT(..) you would call target_link_libraries(... rpclib::rpc)
        #but when adding a subdirectory all the targets go into the global namespace so its
        #just rpc
        target_link_libraries(${TARGET_NAME_PARAM} INTERFACE rpc)
    endif()
endfunction()
