cmake_minimum_required(VERSION 3.1)

project(bufstack)

set(MAIN_TARGET bufstack)

#need to use our FindMsgpack.cmake script
list(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")

set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 11)

find_package(Msgpack 1.0.0 REQUIRED)

FILE(GLOB MAIN_TARGET_SRCS src/*.cpp src/*.c src/*.cxx)

add_executable(${MAIN_TARGET} ${MAIN_TARGET_SRCS})

target_include_directories(${MAIN_TARGET} SYSTEM PRIVATE ${MSGPACK_INCLUDE_DIRS})
target_link_libraries(${MAIN_TARGET} PRIVATE ${MSGPACK_LIBRARIES})
