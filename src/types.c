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
  return (tree) (keyptr - (int) ((size_t) &(x.base.code) - (size_t) &x));
}

/* The key used in the type hash-table have a fixed length which can
   be obtained by calling this function.  */
static inline size_t
hash_key_length ()
{
  struct tree_type_node x;
  return offsetof (struct tree_type_node, size) + sizeof (x.size)
	 - (offsetof (struct tree_type_node, base)
	    + offsetof (struct tree_base, code));
}

#undef HASH_FIND_IN_BKT
#define HASH_FIND_IN_BKT(tbl,hh,head,keyptr,keylen_in,out)			  \
do {										  \
  if (head.hh_head)								  \
    DECLTYPE_ASSIGN(out,ELMT_FROM_HH(tbl,head.hh_head));			  \
  else										  \
    out=NULL;									  \
										  \
  while (out)									  \
    {										  \
      if (out->hh.keylen == keylen_in)						  \
	{									  \
	  tree p = get_tree_from_key (out->hh.key);				  \
	  tree q = get_tree_from_key (keyptr);					  \
	  if (((HASH_KEYCMP (out->hh.key, keyptr, keylen_in)) == 0)		  \
	      && tree_compare (TYPE_DIM (p), TYPE_DIM (q))			  \
	      && tree_compare (TYPE_SHAPE (p), TYPE_SHAPE (q)))			  \
	    break;								  \
	}									  \
      if (out->hh.hh_next)							  \
	DECLTYPE_ASSIGN (out, ELMT_FROM_HH (tbl, out->hh.hh_next));		  \
      else									  \
	out = NULL;								  \
    }										  \
} while (0)

/* Global hash-table for types defined in the program.  */
struct tree_type_node *type_table = NULL;


static struct tree_type_node *types_add_type (tree);
static struct tree_type_node *types_find_in_table (tree);


/* Data structures related to types initialization.  */
void
types_init ()
{
  global_tree[TG_B_TYPE] = types_assign_type (make_type (B_TYPE));
  global_tree[TG_N_TYPE] = types_assign_type (make_type (N_TYPE));
  global_tree[TG_R_TYPE] = types_assign_type (make_type (R_TYPE));
  global_tree[TG_Z_TYPE] = types_assign_type (make_type (Z_TYPE));
}

/* Add a new type to hash table.  */
static struct tree_type_node *
types_add_type (tree el)
{
  enum tree_code code = TREE_CODE (el);
  assert (TREE_CODE_CLASS (code) == tcl_type, "code class has to be a type");
  
  if (TREE_CODE (el) != FUNCTION_TYPE)
    {
      HASH_ADD_KEYPTR (hh, type_table, &(el->base.code), hash_key_length (),
	  		   (struct tree_type_node *) el);
    }
  else
    {
      /* We assign a list in the FUNCTION_TYPE node.  */
      struct tree_type_node *ret = NULL;
      HASH_FIND (hh, type_table, (char *) &(el->base.code), hash_key_length (),
	     ret);
      if (ret == NULL)
	{
	  TYPE_LIST (el) = make_tree_list ();
	  tree_list_append (TYPE_LIST (el), el);
	}
      else
	tree_list_append (ret->list, el);
    }
  return ((struct tree_type_node *) el);
}

/* Try to find an element in table. We construct a tree for this. We don't
   destroy the old type.  */
static struct tree_type_node *
types_find_in_table (tree el)
{
  enum tree_code code = TREE_CODE (el);
  struct tree_type_node *ret = NULL;
  struct tree_list_element *type_el;
  assert (TREE_CODE_CLASS (code) == tcl_type, "code class has to be a type");

  HASH_FIND (hh, type_table, (char *) &(el->base.code), hash_key_length (),
	     ret);
 
  /* Function types are stored in a single list in the hash table.  */
  if (ret && TREE_CODE (el) == FUNCTION_TYPE)
    {
      DL_FOREACH (TREE_LIST (ret->list), type_el)
	{
	  if (tree_compare (TYPE_FUNCTION_ARGS (type_el->entry),
			    TYPE_FUNCTION_ARGS (el))
	   && tree_compare (TYPE_FUNCTION_RET (type_el->entry),
			    TYPE_FUNCTION_RET (el)))
	    return &type_el->entry->type_node;
	   
	}
    }
  return ret;
}

/* Try to find a corresponded type, if not found -- add a new one.  */
tree
types_assign_type (tree el)
{
  struct tree_type_node *found = types_find_in_table (el);
  return (tree) (found ? found : types_add_type (el));
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
