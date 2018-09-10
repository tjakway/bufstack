#-D_GLIBCXX_DEBUG forces clang to emit debug information for libstdc++
#this is needed because clang otherwise sometimes assumes that libstdc++
#provides debug information even when it doesn't
#this is set globally because it affects all targets
#https://stackoverflow.com/questions/41745527/cannot-view-stdstring-when-compiled-with-clang
#
#TODO: refactor out the CMAKE_BUILD_TYPE check to a separate function that also
#checks for debug-related targets like DebugWithWarnings[Optimized]
if (CMAKE_BUILD_TYPE EQUAL "DEBUG" AND "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    set(CMAKE_CXX_FLAGS "-D_GLIBCXX_DEBUG -fstandalone-debug ${CMAKE_CXX_FLAGS}")
endif()
