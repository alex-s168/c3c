find_package(Git QUIET)

set(GIT_HASH "unknown")

if(GIT_FOUND AND EXISTS "${CMAKE_CURRENT_LIST_DIR}/.git")
    execute_process(COMMAND ${GIT_EXECUTABLE} rev-parse HEAD
                    OUTPUT_VARIABLE GIT_HASH
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()

message("Git Hash: ${GIT_HASH}")

file(WRITE ${CMAKE_BINARY_DIR}/git_hash.h "#pragma once\n#define GIT_HASH \"${GIT_HASH}\"\n")
