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

#include "types.h"
#include "tree.h"

struct tree_type_node * type_table;

/* Data structures related to types initialization.  */
void
types_init ()
{
  type_table = NULL;
  types_add_type (B_TYPE, 1);
  types_add_type (N_TYPE, sizeof (unsigned) * 8);
  types_add_type (R_TYPE, sizeof (double) * 8);
  types_add_type (Z_TYPE, sizeof (int) * 8); 
}

/* Add a new type to hash table.  */
tree
types_add_type (enum tree_code code, size_t size)
{
  tree el = NULL;
  size_t keylen = offsetof (struct tree_type_node, hh);

  assert (TREE_CODE_CLASS (code) == tcl_type, "code class has to be a type");
  el = make_type (code);
  TYPE_SIZE (el) = size;

  HASH_ADD_KEYPTR (hh, type_table, &TYPE_HASH (el), keylen, &TYPE_HASH (el));
  return el;
}

struct tree_type_node * 
types_find_in_table (enum tree_code code, size_t size)
{
  tree el = NULL;
  struct tree_type_node* ret = NULL;
  size_t keylen = offsetof (struct tree_type_node, hh);

  assert (TREE_CODE_CLASS (code) == tcl_type, "code class has to be a type");
  el = make_type (code);
  TYPE_SIZE (el) = size;

  HASH_FIND (hh, type_table, &TYPE_HASH (el), keylen, ret);
  return ret;
}

