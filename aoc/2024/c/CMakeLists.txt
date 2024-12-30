# Specify the minimum CMake version
cmake_minimum_required(VERSION 3.28)

# Set the project name
project(aoc2024 C)

# Enable compile commands export
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Set common compile flags
set(COMMON_COMPILE_FLAGS -Wall -Wextra -pedantic)

# Check if the environment variable TARGET is set to "prod"
if(DEFINED ENV{TARGET} AND "$ENV{TARGET}" STREQUAL "prod")
    # If it's 'prod', don't add -Werror and -g
    message(STATUS "=> Building for production: skipping -Werror and -g")
    else()
    message(STATUS "=> Building for development, adding -Werror and -g")
    # If it's not 'prod', add -Werror and -g
    list(APPEND COMMON_COMPILE_FLAGS -Werror -g)
endif()

# List all the source files (executables to create)
set(SOURCES day01.c day06.c)

# Loop through each source file and create an executable
foreach(SOURCE ${SOURCES})
    # Get the executable name by removing the extension
    get_filename_component(EXECUTABLE ${SOURCE} NAME_WE)

    # Add the executable target
    add_executable(${EXECUTABLE} ${SOURCE})

    # Apply the common compile flags to this target
    target_compile_options(${EXECUTABLE} PRIVATE ${COMMON_COMPILE_FLAGS})
endforeach()
