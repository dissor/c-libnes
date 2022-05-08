add_executable(main "")

target_sources(main
    PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/main.c
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}/main.h
)

target_include_directories(main
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
        ${CMAKE_CURRENT_LIST_DIR}/SDL2/include/SDL2
)

target_link_libraries(main
    PUBLIC
        nes
        ${CMAKE_CURRENT_LIST_DIR}/SDL2/bin/SDL2.dll
)