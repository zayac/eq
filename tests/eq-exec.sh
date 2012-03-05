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
# The script below reads the input file matching the lines with `eq-exec'
# prefix and compares an expected output with an actual one. In case of match
# pring return `passed' string, otherwise -- `failed ' string.

texOutput=`cat -n $1 | \
egrep '%[ \t]*eq-exec:' | \
sed -e 's/^[ \t]*\([0-9]*\)[^%]*%[ \t]*eq-exec: \(.*\)/\2/g'`
./eq $1
execOutput=`$2 out.py`
if [ "$execOutput" == "$texOutput" ]
then
  echo "$1 passed"
else
  echo "$1 failed"
fi
exit 0

# For example:
#
# \begin{eqcode}{\mu}{\ }{\ }{\type{Z}}
# a \gets 3 \lend
# \print {a} \lend
# \return {a} \lend
# \end{eqcode}
# % eq-exec: 3
