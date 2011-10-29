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

#include "tree.h"
#include "types.h"
#include "global.h"
#include "uthash.h"

/* The way we store pointer of the key used within the hash table
   makes it very inconvenient to handle the tree-type node of the
   hash-key.  This function gets the tree-node itself from the
   pointer to a hash-key.  */
static inline tree
get_tree_from_key (char *keyptr)
{
  struct tree_type_node x;
  return (tree) (keyptr - (int) ((size_t) & (x.base.code) - (size_t) & x));
}

/* The key used in the type hash-table have a fixed length which can
   be obtained by calling this function.  */
static inline size_t
hash_key_length ()
{
  struct tree_type_node x;
  return offsetof (struct tree_type_node,
		   size) +sizeof (x.size) - (offsetof (struct tree_type_node,
						       base) +
					     offsetof (struct tree_base,
						       code));
}

/* FIXME the formatting of the following macro is incorrect.  */
#undef HASH_FIND_IN_BKT
#define HASH_FIND_IN_BKT(tbl,hh,head,keyptr,keylen_in,out)                       \
do {										 \
  if (head.hh_head) DECLTYPE_ASSIGN(out,ELMT_FROM_HH(tbl,head.hh_head));         \
  else out=NULL;                                                                 \
  while (out) {									 \
    if (out->hh.keylen == keylen_in) {						 \
      tree p = get_tree_from_key (out->hh.key);					 \
      tree q = get_tree_from_key (keyptr);					 \
      if (((HASH_KEYCMP(out->hh.key,keyptr,keylen_in))  == 0)			 \
	   && tree_compare (TYPE_DIM (p), TYPE_DIM (q))				 \
	   && tree_compare (TYPE_SHAPE (p), TYPE_SHAPE (q)))			 \
	break;									 \
    }                                                                            \
    if (out->hh.hh_next) DECLTYPE_ASSIGN(out,ELMT_FROM_HH(tbl,out->hh.hh_next)); \
    else out = NULL;                                                             \
  }\
} while(0)

struct tree_type_node *type_table = NULL;

static struct tree_type_node *types_add_type (enum tree_code, size_t, tree,
					      tree);
static struct tree_type_node *types_find_in_table (enum tree_code, size_t,
						   tree, tree);


/* Data structures related to types initialization.  */
void
types_init ()
{
  global_tree[TG_B_TYPE] = types_assign_type (B_TYPE, 1, NULL, NULL);
  global_tree[TG_N_TYPE] =
    types_assign_type (N_TYPE, sizeof (unsigned) * 8, NULL, NULL);
  global_tree[TG_R_TYPE] =
    types_assign_type (R_TYPE, sizeof (double) * 8, NULL, NULL);
  global_tree[TG_Z_TYPE] =
    types_assign_type (Z_TYPE, sizeof (int) * 8, NULL, NULL);

  /* FIXME Remove this assertion when not needed.  */
  assert (types_find_in_table (B_TYPE, 1, NULL, NULL)
	  && types_find_in_table (N_TYPE, sizeof (unsigned) * 8, NULL, NULL)
	  && types_find_in_table (R_TYPE, sizeof (double) * 8, NULL, NULL)
	  && types_find_in_table (Z_TYPE, sizeof (int) * 8, NULL, NULL),
	  "The basic types were not properly added to the hash-table");

}

/* Add a new type to hash table.  */
static struct tree_type_node *
types_add_type (enum tree_code code, size_t size, tree dim, tree shape)
{
  tree el = NULL;

  assert (TREE_CODE_CLASS (code) == tcl_type, "code class has to be a type");
  el = make_type (code);
  TYPE_SIZE (el) = size;
  TYPE_DIM (el) = dim;
  TYPE_SHAPE (el) = shape;

  HASH_ADD_KEYPTR (hh, type_table, &(el->base.code), hash_key_length (),
		   (struct tree_type_node *) el);
  return ((struct tree_type_node *) el);
}

/* Try to find an element in table. We construct a tree for this. We destroy in
   finally.  */
static struct tree_type_node *
types_find_in_table (enum tree_code code, size_t size, tree dim, tree shape)
{
  tree el = NULL;

  struct tree_type_node *ret = NULL;
  assert (TREE_CODE_CLASS (code) == tcl_type, "code class has to be a type");
  el = make_type (code);
  TYPE_SIZE (el) = size;
  TYPE_DIM (el) = dim;
  TYPE_SHAPE (el) = shape;

  HASH_FIND (hh, type_table, (char *) &(el->base.code), hash_key_length (),
	     ret);
  if (ret == NULL)
    free_tree_type (el, false);
  else
    free_tree_type (el, true);
  return ret;
}

/* Try to find a corresponded type, if not found -- add a new one.  */
tree
types_assign_type (enum tree_code code, size_t size, tree dim, tree shape)
{
  struct tree_type_node *found = types_find_in_table (code, size, dim, shape);
  return (tree) (found ? found : types_add_type (code, size, dim, shape));
}

void
types_finalize ()
{
  struct tree_type_node *current, *tmp;
  HASH_ITER (hh, type_table, current, tmp)
  {
    HASH_DEL (type_table, current);
    free_tree_type ((tree) current, true);
  }
  type_table = NULL;
}
