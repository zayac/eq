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

/* Data structures related to types initialization.  */
void
types_init ()
{
  type_name_list = make_tree_list ();
  type_size_list = make_tree_list ();
  type_table = NULL;
}

/* Add a new type to hash table.  */
struct type_hash_table* 
types_add_type (tree name, tree size)
{
  struct type_table_key *key;   
  struct type_hash_table *el;

  key = (struct type_table_key*) malloc (sizeof (struct type_table_key));
  el = (struct type_hash_table*) malloc (sizeof (struct type_hash_table));

  key->name = name;
  key->size = size;

  HASH_ADD (hh, type_table, key, sizeof (struct type_table_key), el);
  return el;
}

void 
types_add_primitive_types ()
{
  types_set_simple_type (STRING_TYPE, 0);
  types_set_simple_type (Z_TYPE, sizeof (int));
  types_set_simple_type (R_TYPE, sizeof (double));
  types_set_simple_type (N_TYPE, sizeof (unsigned));
  types_set_simple_type (B_TYPE, sizeof (bool));
}

/* This function is used to set simple types in an easy way.
   If code belongs to tcl_type and size is not equal zero, we try to
   find the relevant entries in type_name_list and type_size_list.  If
   there are no such entries, we create them. Then we look for an
   entry in type hash table.  If there is no such an entry, we create
   it.  If code is set to EMPTY_MARK, we try to find it in a list and
   in case of unluck add a new entry to the size list.  If size is set
   to zero, we try to find it in a list and in case of unluck add a
   new entry to the name list.  */
struct type_hash_table*
types_set_simple_type (enum tree_code code, size_t size)
{
  struct type_table_key key;
  struct type_hash_table * type_el = NULL;
  struct tree_list_element * el = NULL;

  if (code == EMPTY_MARK && size == 0)
    return NULL;

  assert (TREE_CODE_CLASS (code) == tcl_type || code == EMPTY_MARK, 
	  "code has to belong to tcl_type class or be an EMPTY_MARK");
  
  if (code != EMPTY_MARK)
    {
      /* Try to find an entry in type_name_list.  */
      key.name = NULL;
      DL_FOREACH (TREE_LIST (type_name_list), el)
	{
	  if (TREE_CODE (el->entry) == code)
	    {
	      key.name = el->entry;
	      break;
	    }
	}

      /* Create a new entry in the list for name.  */
      if (key.name == NULL)
	{
	  key.name = make_type (code);
	  tree_list_append (type_name_list, key.name);
	}

      if (size == 0)
	return NULL;
    }

  if (size != 0)
    {
      /* Try to find an entry in type_size_list.  */
      el = NULL;
      key.size = NULL;
      DL_FOREACH (TREE_LIST (type_size_list), el)
	{
	  if (TREE_CODE (el->entry) == INTEGER_CST &&
	      TREE_INTEGER_CST (el->entry) == size)
	    {
	      key.size = el->entry;
	      break;
	    }
	}

      /* Create a new entry in the list for size.  */
      if (key.size == NULL)
	{
	  key.size = make_type (code);
	  tree_list_append (type_size_list, key.size);
	}
    
      if (code == EMPTY_MARK)
	return NULL;
    }


  HASH_FIND (hh, type_table, &key, sizeof (struct type_table_key), type_el);
  if (type_el != NULL)
    return type_el;
  else
    return types_add_type (key.name, key.size);  
}

struct type_hash_table * 
types_find_in_table (tree name, tree size)
{
  struct type_table_key key;
  struct type_hash_table* type_el;
  key.name = name;
  key.size = size;

  HASH_FIND (hh, type_table, &key, sizeof (struct type_table_key), type_el);
  return type_el;
}

tree
find_primitive_size_in_list (size_t size)
{
  struct tree_list_element *el = NULL;
  DL_FOREACH (TREE_LIST (type_size_list), el)
    {
      if (TREE_CODE (el->entry) == INTEGER_CST && 
	  TREE_INTEGER_CST (el->entry) == size)
	return el->entry;
    }
  return NULL;
}

tree
find_primitive_name_in_list (enum tree_code name)
{
  struct tree_list_element *el = NULL;
  DL_FOREACH (TREE_LIST (type_name_list), el)
    {
      if (TREE_CODE_CLASS (TREE_CODE (el->entry)) == tcl_type && 
	  TREE_CODE (el->entry) == name)
	return el->entry;
    }
  return NULL;
}


