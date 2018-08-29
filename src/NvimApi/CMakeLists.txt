cmake_minimum_required(VERSION 3.1)
project(nvim_api)

FILE(GLOB NVIM_API_LIB_SRCS *.cpp *.c *.cxx)

add_library(${NVIM_API_LIB_TARGET} ${NVIM_API_LIB_SRCS})
target_include_directories(${NVIM_API_LIB_TARGET} 
    SYSTEM PUBLIC ${CMAKE_SOURCE_DIR}/include)

depends_msgpack(${NVIM_API_LIB_TARGET})
depends_rpclib(${NVIM_API_LIB_TARGET})
depends_spdlog(${NVIM_API_LIB_TARGET})

process_warnings(${NVIM_API_LIB_TARGET})
