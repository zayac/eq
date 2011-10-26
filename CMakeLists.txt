#  Copyright (c) 2011 Artem Shinkarov <artyom.shinkaroff@gmail.com>
#                     Pavel Zaichenkov <zaichenkov@gmail.com>
#
#  Permission to use, copy, modify, and distribute this software for any
#  purpose with or without fee is hereby granted, provided that the above
#  copyright notice and this permission notice appear in all copies.
#
#  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
#  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
#  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
#  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
#  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
#  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
#  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

cmake_minimum_required (VERSION 2.6)

# Name of the project
project (eq C)

# Uncomment this if you want to have a verbose
# makefile. It is useful for debugging. 
# set (CMAKE_VERBOSE_MAKEFILE ON)

option (BUILD_LEXER "compiling lexer")
option (DEBUG_ENABLE "set debugging flag")

# Set BUILD_LEXER variable value to ON if you want to compile the lexer only
set (BUILD_LEXER OFF)
set (DEBUG_ENABLE ON)

set (CFLAGS "-g -gdwarf-2 -g3 -Wall -std=c99 -D_BSD_SOURCE")

include (cmake/version.cmake)

configure_file (
  "${PROJECT_SOURCE_DIR}/src/config.h.in"
  "${PROJECT_BINARY_DIR}/config.h"
)

include_directories ("${PROJECT_BINARY_DIR}")

add_subdirectory (src)

add_definitions (${CFLAGS})
if (DEBUG_ENABLE)
  add_definitions(-DDEBUG_ENABLE)
endif()
if (BUILD_LEXER)
  add_definitions(-DLEXER_BINARY)
  add_executable (eq src/lex.c)
else()
  add_executable (eq src/main.c)
  target_link_libraries (eq eqlib)
endif()


# Testing routines
enable_testing ()

macro (xtest arg)
  add_test (test_${arg} eq ${arg})
  set_tests_properties (test_${arg}
  PROPERTIES 
    PASS_REGULAR_EXPRESSION "finished parsing"
    FAIL_REGULAR_EXPRESSION "^error:")
endmacro (xtest)

file (GLOB_RECURSE testfiles "tests/parser_tests/*.tex")
foreach (test ${testfiles})
  xtest (${test})
endforeach ()
