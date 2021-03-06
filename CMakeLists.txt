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

option (BUILD_LEXER "compiling lexer" OFF)
option (FIRST_CLASS_FUNCTIONS "functions as first-class objects" ON)
option (CFG_OUTPUT "output control flow graph" ON)
option (RELASE "build the compiler with highest optimisations" OFF)

# A list of available backends
set (BACKENDS python sac)

# Select one of backends
if (NOT BACKEND_LANG)
  list(GET BACKENDS 0 BACKEND_LANG)
else()
  list(FIND BACKENDS ${BACKEND_LANG} contains)
  if (${contains} LESS 0)
    message(FATAL_ERROR "`${BACKEND_LANG}' backend is not supported.")
  endif()
endif()

# Check valgrind existence
set (VALGRIND_EXISTS 0)
execute_process(COMMAND valgrind --version
  RESULT_VARIABLE valgrind_result
  OUTPUT_VARIABLE valgrind_out)
if (NOT ${valgrind_result} EQUAL 0)
  message(STATUS "vailgrind is not found. You will be unable to "
		 "run 'memory_test' test")
else()
  set (VALGRIND_EXISTS 1)
endif()

if (RELEASE)
  set (CFLAGS "-O3 -fomit-frame-pointer -Wall -Wextra -Wstrict-prototypes "
	      "-pedantic -std=c99 -D_BSD_SOURCE"
	      " -D_GNU_SOURCE -pedantic -fPIC")
  message(STATUS "Compiling release version")
else()
  set (CFLAGS "-g -gdwarf-2 -g3 -Wall -Wextra -Wstrict-prototypes "
	      "-pedantic -std=c99 -D_BSD_SOURCE"
	      " -D_GNU_SOURCE -pedantic -fPIC")
  message(STATUS "Compiling debug version")
endif()

# Custom messages
if (BUILD_LEXER)
  message(STATUS "Only lexer is being built")
endif()


message(STATUS "Control flow graph output is ${CFG_OUTPUT}")

message(STATUS "${BACKEND_LANG} as a backend is used")

include (cmake/version.cmake)

configure_file (
  "${PROJECT_SOURCE_DIR}/src/config.h.in"
  "${PROJECT_BINARY_DIR}/config.h"
)

if (NOT FIRST_CLASS_FUNCTIONS)
  message (STATUS "First-class functions are disabled")
  add_definitions(-DDISABLE_FCF)
endif()

if (CFG_OUTPUT)
  add_definitions(-DCFG_OUTPUT)
endif()

include_directories ("${PROJECT_BINARY_DIR}")
include_directories ("src")
include_directories ("src/backend_${BACKEND_LANG}")

add_definitions (${CFLAGS})
if (BUILD_LEXER)
  add_definitions(-DLEXER_BINARY)
  add_executable (eq src/lex.c)
else()
  add_subdirectory (src)
  add_executable (eq src/main.c)
  target_link_libraries (eq eqlib)
endif()


# Installing eq binary, libraries and include files.
set (CMAKE_INSTALL_PREFIX "$ENV{HOME}/local")
if (BUILD_LEXER)
  install (TARGETS eq DESTINATION bin)
else()
  install (TARGETS eq DESTINATION bin)
  file (GLOB headers "${PROJECT_SOURCE_DIR}/src/*.h")
  file (GLOB defs "${PROJECT_SOURCE_DIR}/src/*.def")
  install (FILES ${headers} ${defs} DESTINATION include/eq)
endif()




# Testing routines
enable_testing ()

# Add tests that must run and should not produce any errors
macro (run_test arg opt)
  add_test (run_test_${arg} ${PROJECT_SOURCE_DIR}/tests/eq-run.sh 
	    ${PROJECT_BINARY_DIR}/eq ${arg} ${opt})
  set_tests_properties (run_test_${arg}
  PROPERTIES 
    PASS_REGULAR_EXPRESSION "finished compiling"
    FAIL_REGULAR_EXPRESSION "^error:")
endmacro (run_test)

# Add tests that produce errors and have appropriate error-annotations
macro (error_test arg opt)
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

  add_test (error_test_${arg} eq ${opt} ${arg})
  set_tests_properties (error_test_${arg}
  PROPERTIES
    PASS_REGULAR_EXPRESSION ${out})
endmacro(error_test)

macro (cfg_test arg opt)
  add_test (cfg_test_${arg} ${PROJECT_SOURCE_DIR}/tests/eq-cfg.sh ${arg})
  set_tests_properties (cfg_test_${arg}
  PROPERTIES
    PASS_REGULAR_EXPRESSION ".*${arg} passed.*")
endmacro(cfg_test)

macro (exec_test arg opt)
  add_test (exec_test_${arg} ${PROJECT_SOURCE_DIR}/tests/eq-exec.sh ${arg}
  ${BACKEND_LANG})
  set_tests_properties (exec_test_${arg}
  PROPERTIES
    PASS_REGULAR_EXPRESSION ".*${arg} passed.*")
endmacro(exec_test)

# include files for parser testing
file (GLOB run_tests "tests/parser_tests/*.tex")
set (opt "-Bparser")
foreach (test ${run_tests})
  run_test (${test} ${opt})
endforeach ()

# include files for error-testing
file (GLOB_RECURSE error_tests "tests/parser_tests/fail_tests/*.tex")
set (opt "-Bparser")
foreach (test ${error_tests})
  error_test (${test} ${opt})
endforeach ()

#include files for typechecker testing
file (GLOB run_tests "tests/type_tests/*.tex")
set (opt "-Btypecheck")
foreach (test ${run_tests})
  run_test (${test} ${opt})
endforeach ()

# include files for error-testing
file (GLOB_RECURSE error_tests "tests/type_tests/fail_tests/*.tex")
set (opt "-Btypecheck")
foreach (test ${error_tests})
  error_test (${test} ${opt})
endforeach ()


if ("${BACKEND_LANG}" STREQUAL "python")
  # include files for error-testing
  file (GLOB_RECURSE error_tests "tests/codegen_tests_python/fail_tests/*.tex")
  foreach (test ${error_tests})
    error_test (${test} "")
  endforeach ()
endif()

# include files for flow graph testing
file (GLOB cfg_tests "tests/cfg_tests/*.tex")
foreach (test ${cfg_tests})
  cfg_test (${test} "")
endforeach ()

# include files for codegen testing
file (GLOB run_tests "tests/codegen_tests_${BACKEND_LANG}/*.tex")
foreach (test ${run_tests})
  run_test (${test} "")
endforeach ()

# include files for codegen testing
file (GLOB exec_tests "tests/codegen_tests_${BACKEND_LANG}/*.tex")
foreach (test ${exec_tests})
  exec_test (${test} "")
endforeach ()

# Add tests that must run and should not produce any errors
macro (memory_test arg opt)
  add_test (memory_test_${arg} ${PROJECT_SOURCE_DIR}/tests/eq-memory.sh
	    ${PROJECT_BINARY_DIR}/eq ${arg})
  set_tests_properties (memory_test_${arg}
  PROPERTIES 
    PASS_REGULAR_EXPRESSION "no leaks are possible"
    FAIL_REGULAR_EXPRESSION "definitely lost:|Invalid|uninitialised")
endmacro (memory_test)

# include files for memory testing
if (${VALGRIND_EXISTS} EQUAL 1)
  file (GLOB_RECURSE memory_tests "tests/codegen_tests_${BACKEND_LANG}/*.tex"
				  "tests/parser_tests/*.tex"
				  "tests/type_tests/*.tex")
  foreach (test ${memory_tests})
    memory_test (${test} "")
  endforeach ()
endif()
