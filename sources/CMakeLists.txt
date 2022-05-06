add_library(nes "")

target_sources(nes
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/bus.c
        ${CMAKE_CURRENT_LIST_DIR}/cpu.c
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}/bus.h
        ${CMAKE_CURRENT_LIST_DIR}/cpu.h
)

target_include_directories(nes
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
)