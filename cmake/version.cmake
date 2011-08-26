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

if (git_exec)
    execute_process (
      COMMAND "${git_exec}" log --pretty=format:'%ci' -n1
      WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
      OUTPUT_VARIABLE commit_date
      OUTPUT_STRIP_TRAILING_WHITESPACE
      ERROR_QUIET
    )
endif ()

set (COMMIT_DATE "${commit_date}")

