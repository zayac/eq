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

/* Lists of pointers to store names and sizes of types.  */
extern tree type_name_list;
extern tree type_size_list;

/* A structure which is used as the key for type table.  */
struct type_table_key
{
  tree name;
  tree size;
};

/* A hash table for storing types.  */
struct type_hash_table
{
  struct type_table_key key;
  UT_hash_handle hh;
};

/* A global table to store types.  */
struct type_hash_table * type_table;

void types_init ();
struct type_hash_table*  types_add_type (tree, tree);
void types_add_primitive_types ();
struct type_hash_table* types_set_simple_type (enum tree_code, size_t);
struct type_hash_table* types_find_in_table (tree, tree);
tree find_primitive_size_in_list (size_t);
tree find_primitive_name_in_list (enum tree_code);

#endif /* __TYPES_H__  */
