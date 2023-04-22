cmake_minimum_required(VERSION 3.21)

# we'll generate these by default
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

project (clox_bytecode_interpreter LANGUAGES C)

# considering the purpose, we'll do Debug by default
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug)
endif()

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")

# Omitting Werror during initial development for ease of checkin
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -Wpedantic")

# requires 3.21 for c17
set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

if(CMAKE_BUILD_TYPE STREQUAL Debug)
    message("Debug build: using CMAKE_C_FLAGS_DEBUG: ${CMAKE_C_FLAGS_DEBUG}")
    add_compile_definitions(DEBUG_TRACE_EXECUTION)
    add_compile_definitions(DEBUG_PRINT_CODE)
elseif(CMAKE_BUILD_TYPE STREQUAL Release)
    message("Release build: using CMAKE_C_FLAGS_RELEASE ${CMAKE_C_FLAGS_RELEASE}")
endif()

# should also enable verbosity when using Ninja (https://github.com/ninja-build/ninja/issues/900)
set(CMAKE_VERBOSE_MAKEFILE ON)

# All corresponding source files
set(SOURCES
    src/scanner.c
    src/compiler.c
    src/vm.c
    src/chunk.c
    src/debug.c
    src/memory.c
    src/value.c
    src/main.c
)

add_executable(clox_bytecode_interpreter ${SOURCES})

# header files in `include` directory
target_include_directories(clox_bytecode_interpreter
    PRIVATE
        ${PROJECT_SOURCE_DIR}/include
)
