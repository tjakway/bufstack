function(depends_msgpack TARGET_NAME_PARAM)
    if(USE_SYSTEM_MSGPACK)
        find_package(Msgpack 5.0.0 REQUIRED)

        target_include_directories(${TARGET_NAME_PARAM} SYSTEM INTERFACE ${MSGPACK_INCLUDE_DIRS})
        target_link_libraries(${TARGET_NAME_PARAM} INTERFACE ${MSGPACK_LIBRARIES})
    else()
        #make sure we get msgpack with C++11
        set(MSGPACK_CXX11 ON CACHE BOOL "Require C++11" FORCE)

        add_subdirectory(${CMAKE_SOURCE_DIR}/lib/msgpack-c ${CMAKE_BINARY_DIR}/msgpack-c)

        target_include_directories(${TARGET_NAME_PARAM} SYSTEM PUBLIC
            ${CMAKE_SOURCE_DIR}/lib/msgpack-c/include)
        target_link_libraries(${TARGET_NAME_PARAM} INTERFACE msgpackc-static)
    endif()
endfunction()
