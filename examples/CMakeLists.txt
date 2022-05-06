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
)

target_link_libraries(main
    PUBLIC
        nes
)