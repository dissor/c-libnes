cmake_minimum_required(VERSION 3.5 FATAL_ERROR)
project(c-libnes LANGUAGES C)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g")


add_subdirectory(sources)
add_subdirectory(examples)