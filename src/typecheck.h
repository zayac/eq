/* Copyright (c) 2011 Artem Shinkarov <artyom.shinkaroff@gmail.com>
		      Pavel Zaichenkov <zaichenkov@gmail.com>

   Permission to use, copy, modify, and distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.
   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. */

#ifndef __TYPECHECK_H__
#define __TYPECHECK_H__

int typecheck ();
int typecheck_function (tree);
int typecheck_stmt_list (tree, tree, tree, tree);
int typecheck_stmt (tree, tree, tree, tree);
int typecheck_expression (tree, tree, tree, tree);
int typecheck_type (tree, tree, tree, tree);
int typecheck_recurrent (tree);
int typecheck_generator (tree, tree, tree, tree);
int typecheck_function_call_args (tree, tree, tree, tree, tree);
int typecheck_function_call (tree, tree, tree, tree);
int typecheck_genarray (tree, tree, tree, tree);
int typecheck_lower (tree, tree, tree, tree, bool);
#endif /* __TYPECHECK_H__ */
