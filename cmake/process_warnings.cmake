
#divide debug fields into common (i.e. non compiler-specific) debug flags, GCC-specific flags and clang-specific flags
########################################
#cmake list of flags
set(COMMON_DEBUG_FLAGS -g -Wall -Wextra -Wundef -Wcast-qual -Wconversion -Wformat=2 -Wshadow -ftrapv -Wuninitialized -Winit-self -Wcast-align -Wwrite-strings)
set(SUPPRESS_DEBUG_FLAGS -Wno-shadow)

#Apple LLVM version 6.1.0 (clang-602.0.53) (based on LLVM 3.6.0svn) doesn't support -faddress=sanitize
#NOTE THAT VALGRIND WILL NOT WORK WITH -fsanitize=address (the other flags do not seem to pose a problem)
set(CLANG_SANITIZE_ADDRESS_FLAG "-fsanitize=address")
set(GCC_OPTIMIZE_DEBUG "-Og")
########################################

function(SuppressDebugWarnings tgt)
    target_compile_options(${tgt} PRIVATE ${SUPPRESS_DEBUG_FLAGS})
endfunction()

function(EnableDebugWarnings tgt)
    target_compile_definitions(${tgt} PRIVATE "-DDEBUG") 

    target_compile_options(${tgt} PRIVATE ${COMMON_DEBUG_FLAGS})

    #add clang-specific debugging flags
    #note: clang with apple modifications is named AppleClang (which will fail STREQUAL "Clang")
    #use MATCHES "Clang" to check for either kind of clang
    #see http://stackoverflow.com/questions/10046114/in-cmake-how-can-i-test-if-the-compiler-is-clang
    if((${CMAKE_CXX_COMPILER_ID} MATCHES "Clang") AND NOT ${APPLE})
        message("Building with clang on a non-Apple machine.  Enabling -faddress=sanitize.")
        target_compile_options(${tgt} PRIVATE ${CLANG_SANITIZE_ADDRESS_FLAG})
    endif()
endfunction()

function(process_warnings tgt)
    if(("${CMAKE_BUILD_TYPE}" STREQUAL "DEBUG") OR ("${CMAKE_BUILD_TYPE}" STREQUAL "Debug"))
        message("Building in debug mode without warnings.")
        message("To enable warnings, pass -DCMAKE_BUILD_TYPE=DebugWithWarnings")
        #see http://stackoverflow.com/questions/5352074/how-to-create-a-c-define-for-a-certain-target-using-cmake
        target_compile_definitions(${tgt} PRIVATE "-DDEBUG") 
        SuppressDebugWarnings(${tgt})
    elseif("${CMAKE_BUILD_TYPE}" STREQUAL "DebugWithWarnings")
        message("Building in debug mode with warnings.")
        message("To disable warnings, pass -DCMAKE_BUILD_TYPE=Debug")
        EnableDebugWarnings(${tgt})
    #GCC has an -Og option to enable optimizations that won't interfere with debugging
    elseif("${CMAKE_BUILD_TYPE}" STREQUAL "DebugWithWarningsOptimized")
        EnableDebugWarnings(${tgt})
        if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
            message("GNU compiler detected, building in optimized debug mode with warnings.")
            target_compile_options(${tgt} PRIVATE ${GCC_OPTIMIZE_DEBUG})
        else()
            message("WARNING: COMPILER IS NOT GNU" WARNING)
            message("CANNOT BUILD IN OPTIMIZED DEBUG MODE!" WARNING)
            message("BUILDING IN DebugWithWarnings MODE!" WARNING)
        endif()
    else()
        message("Not building in debug mode.")
        #disables assertions
        #see http://stackoverflow.com/questions/5354314/how-to-completely-disable-assertion
        #mandated by POSIX
        target_compile_definitions(${tgt} PRIVATE "-DNDEBUG")
    endif()
endfunction(process_warnings)
