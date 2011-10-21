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
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  */

#ifndef __TYPES_H__
#define __TYPES_H__

#include "tree.h"
#include "uthash.h"

extern tree type_basic[4];

#define BASIC_TYPE_B (type_basic[0])
#define BASIC_TYPE_N (type_basic[1])
#define BASIC_TYPE_R (type_basic[2])
#define BASIC_TYPE_Z (type_basic[3])

/* A global table to store types.  */
extern struct tree_type_node * type_table;

void types_init ();
struct tree_type_node* types_add_type (enum tree_code, size_t, tree, tree);
struct tree_type_node* types_find_in_table (enum tree_code, size_t, tree, tree);
UT_array* gen_hash_data (enum tree_code, size_t, tree, tree);
tree types_assign_type (enum tree_code, size_t, tree, tree);
void types_free_type (struct tree_type_node *);
void types_finalize ();

#endif /* __TYPES_H__  */
