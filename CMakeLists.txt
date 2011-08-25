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

# Version numbers
set (VERSION_MAJOR 0)
set (VERSION_MINOR 1)
set (VERSION_PATCH 1)
set (VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}")
set (COMMIT_DATE "???")

# This is a unix hack to get a revision number from git
# would not work anywhere where wc is not present.
find_program(git_exec
  NAMES
    git git.cmd
  HINTS
    ENV PATH
  DOC "git installation path"
)

find_program(wc_exec
  NAMES
    wc
  HINTS
    ENV PATH
  DOC "unix word-count utility"
)

if (GIT_EXEC)
  if (WC_EXEC)
    execute_process (
      COMMAND "${git_exec}" log --pretty=format:'%ci' -n1
      WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
      OUTPUT_VARIABLE commit_date
      OUTPUT_STRIP_TRAILING_WHITESPACE
      ERROR_QUIET
    )
  endif ()
endif ()

set (COMMIT_DATE "${commit_date}")


configure_file (
  "${PROJECT_SOURCE_DIR}/config.h.in"
  "${PROJECT_BINARY_DIR}/config.h"
)

include_directories ("${PROJECT_BINARY_DIR}")

add_definitions(-g -Wall -std=c99)

# EQ library files
set (eqlib_src
global.c  lex.c  
parser.c  print.c  
tree.c)
add_library (eqlib ${eqlib_src})

add_executable (eq main.c)
target_link_libraries (eq eqlib)


# Testing routines
enable_testing ()

macro (xtest arg)
  add_test (test_${arg} eq ${arg})
  set_tests_properties (test_${arg}
  PROPERTIES 
    PASS_REGULAR_EXPRESSION "finished parsing"
    FAIL_REGULAR_EXPRESSION "^error:")
endmacro (xtest)

file (GLOB_RECURSE testfiles "${PROJECT_SOURCE_DIR}/tests/parser_tests/*.tex")
foreach (test ${testfiles})
  xtest (${test})
endforeach ()
