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

#ifndef __RECURRENCE_H__
#define __RECURRENCE_H__

#define recurrence_find_min(t) recurrence_find (t, 0)
#define recurrence_find_max(t) recurrence_find (t, 1)

int recurrence_sort (struct tree_list_element*, struct tree_list_element*);
int recurrence_check_relation (tree, tree);
int recurrence_find_max_shift (tree);
int recurrence_validate_indexes (tree, tree, int, int);
int recurrence_validate (tree);
bool recurrence_is_constant_expression (tree);

#endif /* __RECURRENCE_H__ */
