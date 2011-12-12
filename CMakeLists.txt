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

# A list of available backends
set (BACKENDS python haskell)

# Select one of backends
list(GET BACKENDS 0 BACKEND_LANG)

set (CFLAGS "-g -gdwarf-2 -g3 -Wall -std=c99 -D_BSD_SOURCE"
	    " -D_GNU_SOURCE -pedantic")

include (cmake/version.cmake)

configure_file (
  "${PROJECT_SOURCE_DIR}/src/config.h.in"
  "${PROJECT_BINARY_DIR}/config.h"
)

include_directories ("${PROJECT_BINARY_DIR}")
include_directories ("src")
include_directories ("src/backend_${BACKEND_LANG}")
add_subdirectory (src)

add_definitions (${CFLAGS})
if (BUILD_LEXER)
  add_definitions(-DLEXER_BINARY)
  add_executable (eq src/lex.c)
else()
  add_executable (eq src/main.c)
  target_link_libraries (eq eqlib)
endif()


# Testing routines
enable_testing ()

# Add tests that must run and should not produce any errors
macro (run_test arg opt)
  add_test (NAME run_test_${arg}
	    COMMAND eq ${opt} ${arg})
  set_tests_properties (run_test_${arg}
  PROPERTIES 
    PASS_REGULAR_EXPRESSION "finished compiling"
    FAIL_REGULAR_EXPRESSION "^error:")
endmacro (run_test)

# Add tests that produce errors and have appropriate error-annotations
macro (fail_test arg opt)
  execute_process (COMMAND ${PROJECT_SOURCE_DIR}/tests/eq-error.sh ${arg}
		   OUTPUT_VARIABLE out
  		   ERROR_VARIABLE err
  		   RESULT_VARIABLE res)
  
  if (NOT "${res}" STREQUAL "0")
    message (FATAL_ERROR 
	     "cannot execute "
	     "`${PROJECT_SOURCE_DIR}/tests/eq-error.sh ${arg}'")
  endif ()

  if (NOT "${err}" STREQUAL "")
    message (FATAL_ERROR "cat -n ${arg} returned the following errors\n${err}")
  endif()
  
  if ("${out}" STREQUAL "")
    message ("warning: no eq-error directive found in '${arg}'")
    set (out "error:")
  endif()

  add_test (fail_test_${arg} eq ${opt} ${arg})
  set_tests_properties (fail_test_${arg}
  PROPERTIES
    PASS_REGULAR_EXPRESSION ${out})
endmacro(fail_test)

# include files for parser testing
file (GLOB run_tests "tests/parser_tests/*.tex")
set (opt "-Bparser")
foreach (test ${run_tests})
  run_test (${test} ${opt})
endforeach ()

# include files for error-testing
file (GLOB_RECURSE fail_tests "tests/parser_tests/fail_tests/*.tex")
set (opt "-Bparser")
foreach (test ${fail_tests})
  fail_test (${test} ${opt})
endforeach ()

#include files for typechecker testing
file (GLOB run_tests "tests/type_tests/*.tex")
set (opt "-Btypecheck")
foreach (test ${run_tests})
  run_test (${test} ${opt})
endforeach ()

# include files for error-testing
file (GLOB_RECURSE fail_tests "tests/type_tests/fail_tests/*.tex")
set (opt "-Btypecheck")
foreach (test ${fail_tests})
  fail_test (${test} ${opt})
endforeach ()

# include files for codegen testing
file (GLOB run_tests "tests/codegen_tests_${BACKEND_LANG}/*.tex")
foreach (test ${run_tests})
  run_test (${test} "")
endforeach ()
