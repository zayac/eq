#!/bin/sh

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


# This file is a part of Eq testing routine.
# The script below reads the input file line by line and matches all
# `eq-error:' directives.  After that it saves the line, where the
# directive was found and the text that follows after the directive.
# Finally it converts the list into an appropriate form for cmake. 


cat -n $1 | \
egrep '%\s*eq-error:' | \
sed -e 's/^\s*\([0-9]*\)[^%]*%\s*eq-error:\s*\(.*\)/error:\1.*\2.*/g' | \
sed ':a;N;$!ba;s/\n$//g' | \
sed -e 's/\\/[\\\\]/g'

# For example:
#
#  \begin{eqcode}{testif}{f}{\type{Z}}{\type{Z}}
#          a \gets 5 \lend
#          x \gets 6
#          y \gets b \lend          % eq-error: token `\lend' expected
#          a \gets 3
#  \end{eqcode}                     % eq-error: token `\lend' expected
#
# Outputs:
#error:4.*token `[\\]lend' expected.*
#error:6.*token `[\\]lend' expected.*

