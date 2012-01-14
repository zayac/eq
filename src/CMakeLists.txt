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

add_definitions (${CFLAGS})
# EQ library files
set (eqlib_src
global.c  lex.c  
parser.c  print.c  
tree.c	  matcher.c
types.c	  typecheck.c
backend_${BACKEND_LANG}/codegen.c
xfile.c	  recurrence.c)
add_library (eqlib ${eqlib_src})

