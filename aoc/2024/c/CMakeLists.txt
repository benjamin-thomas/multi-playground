cmake_minimum_required(VERSION 3.28)

project(aoc2024 C)

# For clang-tidy
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(COMMON_COMPILE_FLAGS -Wall -Wextra -pedantic)

if(DEFINED ENV{TARGET} AND "$ENV{TARGET}" STREQUAL "prod")
    message(STATUS "=> Building for production: skipping -Werror and -g")
    else()
    message(STATUS "=> Building for development, adding -Werror and -g")
    list(APPEND COMMON_COMPILE_FLAGS -Werror -g)
endif()

# set(SOURCES day01.c day06.c)
set(SOURCES day01.c)

# Loop through each source file and create an executable
foreach(SOURCE ${SOURCES})
    # Get the executable name by removing the extension
    get_filename_component(EXECUTABLE ${SOURCE} NAME_WE)

    # Add the executable target
    add_executable(${EXECUTABLE} ${SOURCE})

    # Comment out to enable default log level (info)
    target_compile_definitions(${EXECUTABLE} PRIVATE LOG_LEVEL=LOG_LEVEL_TRACE)

    # Apply the common compile flags to this target
    target_compile_options(${EXECUTABLE} PRIVATE ${COMMON_COMPILE_FLAGS})
endforeach()