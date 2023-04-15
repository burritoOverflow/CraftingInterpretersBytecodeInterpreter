cmake_minimum_required(VERSION 3.5)

# we'll generate these by default
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

project (clox_bytecode_interpreter LANGUAGES C)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra") 
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g")
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_RELEASE} -O3")

# should also enable verbosity when using Ninja (https://github.com/ninja-build/ninja/issues/900)
set(CMAKE_VERBOSE_MAKEFILE ON)

# considering the purpose, we'll do Debug by default
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug)
endif()

# All corresponding source files
set(SOURCES
    src/main.c
)

add_executable(clox_bytecode_interpreter ${SOURCES})

# header files in `include` directory
target_include_directories(clox_bytecode_interpreter
    PRIVATE 
        ${PROJECT_SOURCE_DIR}/include
)
