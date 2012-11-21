/* Copyright (c) 2012 Artem Shinkarov <artyom.shinkaroff@gmail.com>
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

#include <stdlib.h>
#include <err.h>

#include "ssa.h"

/* Copy information about variable instances.
   We call this function when we create a new block and variable information
   needs to be transfered between blocks.  */
struct id_defined*
ssa_copy_var_hash (struct id_defined *hash)
{
  struct id_defined *new_hash = NULL, *el, *tmp;
  HASH_ITER (hh, hash, el, tmp)
    {
      struct tree_hash_node *hel, *htmp;
      struct id_defined* el_copy = (struct id_defined*)
				   malloc (sizeof (struct id_defined));
      el_copy->id = el->id;
      el_copy->was_modified = false;
      el_copy->phi_node = NULL;
      HASH_ITER (hh, el->phi_node, hel, htmp)
	{
	  struct tree_hash_node *phi_node = (struct tree_hash_node *)
				      malloc (sizeof (struct tree_hash_node));
	  phi_node->s = hel->s;
	  HASH_ADD_PTR (el_copy->phi_node, s, phi_node);
	}
      HASH_ADD_KEYPTR (hh, new_hash,
		       el_copy->id, strlen (el_copy->id), el_copy);
    }
  return new_hash;
}

/* Add new element to a table where we store variable versions for ssa
   replacement.  */
void
ssa_declare_new_var (basic_block bb, tree var)
{
  char* s;
  struct id_defined *id_el = NULL;
  struct tree_hash_node *phi_node = (struct tree_hash_node *)
			      malloc (sizeof (struct id_defined));

  assert (TREE_CODE (var) == IDENTIFIER, "identifier expected");
  s = TREE_STRING_CST (TREE_ID_NAME (var));
  id_el = (struct id_defined *) malloc (sizeof (struct id_defined));
  memset (id_el, 0, sizeof (struct id_defined));
  /* Initialization.  */
  id_el->id = s;
  phi_node->s = var; 
  HASH_ADD_PTR (id_el->phi_node, s, phi_node);
  HASH_ADD_KEYPTR (hh, bb->var_hash,
		   id_el->id, strlen (id_el->id), id_el);
}

void
ssa_reassign_var (basic_block bb, tree var)
{
  struct id_defined *id_el = NULL;
  char* source_name = TREE_STRING_CST (TREE_ID_NAME (var));
  HASH_FIND_STR (bb->var_hash, source_name, id_el);
  struct tree_hash_node *el, *phi_tmp;
  if (id_el != NULL)
    {
      struct tree_hash_node *phi_node = (struct tree_hash_node *)
				  malloc (sizeof (struct id_defined));
      id_el->was_modified = true;
      HASH_FREE (hh, id_el->phi_node, el, phi_tmp);
      phi_node->s = var;
      HASH_ADD_PTR (id_el->phi_node, s, phi_node);
    }
  else
    ssa_declare_new_var (bb, var);
  return;
}

/* Search subtree for a identifier node. If found, substitute this with a
   redefined identifier.  */
void
ssa_redefine_vars (basic_block bb, tree node, tree assign_node)
{
  enum tree_code code = TREE_CODE (node);
  struct tree_list_element *el;
  int i;

  /* FIXME May be we can avoid this code duplication.  */
  if (code == LIST)
    {
      DL_FOREACH (TREE_LIST (node), el)
	{
	  if (TREE_CODE (el->entry) == IDENTIFIER)
	    {
	      TREE_ID_DEF (el->entry) = assign_node;
	      ssa_reassign_var (bb, el->entry);
	    }
	}
    }
  else if (code == ASSIGN_STMT)
    {
      if (TREE_CODE (TREE_OPERAND (node, 0)) == IDENTIFIER)
	{
	  TREE_ID_DEF (TREE_OPERAND (node, 0)) = assign_node;
	  ssa_reassign_var (bb, TREE_OPERAND (node, 0));
	}
    }
  else
    {
      for (i = 0; i < TREE_CODE_OPERANDS (code); i++)
	{
	  if (TREE_CODE (TREE_OPERAND (node, i)) == IDENTIFIER)
	    {
	      TREE_ID_DEF (TREE_OPERAND (node, 0)) = assign_node;
	      ssa_reassign_var (bb, TREE_OPERAND (node, i));
	    }
	}
    }
}

/* Callback to sort pointers inside utarray.  */
static int pointer_sort(struct tree_hash_node *a, struct tree_hash_node *b)
{
  if (a->s < b->s)
    return -1;
  else if (a->s == b->s)
    return 0;
  else
    return 1;
}

void
ssa_verify_vars (basic_block bb, tree node, tree stmt)
{
  enum tree_code code = TREE_CODE (node);
  struct tree_list_element *el;
  int i;
  /* FIXME May be we can avoid this code duplication.  */
  if (code == LIST)
    {
      DL_FOREACH (TREE_LIST (node), el)
	{
	  if (TREE_CODE (el->entry) == IDENTIFIER)
	    {
	      struct id_defined *id_el = NULL;
	      HASH_FIND_STR (bb->var_hash,
		  TREE_STRING_CST (TREE_ID_NAME (el->entry)), id_el);
	      if (id_el != NULL)
		{
		  struct tree_hash_node *hel, *tmp;
		  HASH_SORT (id_el->phi_node, pointer_sort);
		  /* Create use-def and def-use chain.  */
		  TREE_ID_UD_CHAIN (el->entry) = make_tree_list ();
		  HASH_ITER (hh, id_el->phi_node, hel, tmp)
		    {
		      tree_list_append (TREE_ID_UD_CHAIN (el->entry), hel->s);
		      TREE_STMT_DEF_NUMBER (stmt)++;
		      if (TREE_ID_DU_CHAIN (hel->s) == NULL)
			TREE_ID_DU_CHAIN (hel->s) = make_tree_list ();
		      tree_list_append (TREE_ID_DU_CHAIN (hel->s), 
					el->entry);
		    }
		}
	      TREE_ID_DEF (el->entry) = stmt;
	    }
	  else
	    ssa_verify_vars (bb, el->entry, stmt);
	  if (TREE_CODE (el->entry) == IF_STMT)
	    break;
	}
    }
  else if (code == ASSIGN_STMT)
    {
      if (TREE_CODE (TREE_OPERAND (node, 1)) == IDENTIFIER)
	{
	  struct id_defined *id_el = NULL;
	  HASH_FIND_STR (bb->var_hash,
	      TREE_STRING_CST (TREE_ID_NAME (node)), id_el);
	  if (id_el != NULL)
	    {
	      struct tree_hash_node *hel, *tmp;
	      HASH_SORT (id_el->phi_node, pointer_sort);
	      TREE_ID_UD_CHAIN (TREE_OPERAND (node, 1)) = make_tree_list ();
	      /* Create use-def and def-use chain.  */
	      HASH_ITER (hh, id_el->phi_node, hel, tmp)
		{
		  tree_list_append (TREE_ID_UD_CHAIN (TREE_OPERAND (node, 1)),
				    hel->s);
		  TREE_STMT_DEF_NUMBER (stmt)++;
		  if (TREE_ID_DU_CHAIN (hel->s) == NULL)
		    TREE_ID_DU_CHAIN (hel->s) = make_tree_list ();
		  tree_list_append (TREE_ID_DU_CHAIN (hel->s), 
				    TREE_OPERAND (node, 1));
		}
	    }
	  TREE_ID_DEF (TREE_OPERAND (node, 1)) = stmt;
	}
      else
	ssa_verify_vars (bb, TREE_OPERAND (node, 1), stmt);
      ssa_redefine_vars (bb, TREE_OPERAND (node, 0), node);
#if 0
      if (ssa_is_entry_point (TREE_OPERAND (node, 1), TREE_OPERAND (func, 1)))
	{
	  /* Append statement into function's entry points list.  */
	  if (TREE_FUNC_ENTRY (func) == NULL)
	    TREE_FUNC_ENTRY (func) = make_tree_list ();
	  tree_list_append (TREE_FUNC_ENTRY (func), node);
	}
#endif
    }
  else
    {
      for (i = 0; i < TREE_CODE_OPERANDS (code); i++)
	{
	  /* Avoid verifying a new block.  */
	  if (code == IF_STMT && i > 0)
	    break;
	  if (TREE_CODE (TREE_OPERAND (node, i)) == IDENTIFIER)
	    {
	      struct id_defined *id_el = NULL;
	      HASH_FIND_STR (bb->var_hash,
		  TREE_STRING_CST (TREE_ID_NAME (TREE_OPERAND (node, i))), id_el);
	      if (id_el != NULL)
		{
		  struct tree_hash_node *hel, *tmp;
		  HASH_SORT (id_el->phi_node, pointer_sort);
		  /* Create use-def and def-use chain.  */
		  TREE_ID_UD_CHAIN (TREE_OPERAND (node, i)) = make_tree_list ();
		  HASH_ITER (hh, id_el->phi_node, hel, tmp)
		    {
		      tree_list_append (TREE_ID_UD_CHAIN (
					    TREE_OPERAND (node, i)),
					hel->s);
		      TREE_STMT_DEF_NUMBER (stmt)++;
		      if (TREE_ID_DU_CHAIN (hel->s) == NULL)
			TREE_ID_DU_CHAIN (hel->s) = make_tree_list ();
		      tree_list_append (TREE_ID_DU_CHAIN (hel->s), 
					TREE_OPERAND (node, i));
		    }
		}
	      TREE_ID_DEF (TREE_OPERAND (node, i)) = stmt;
	    }
	  else
	    ssa_verify_vars (bb, TREE_OPERAND (node, i), stmt);
	}
    }
}
