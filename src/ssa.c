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

#include "ssa.h"

/* A hash table for tracking variable redefinitions.  */
struct id_defined *id_definitions = NULL;

UT_icd tree_icd = {sizeof(tree), NULL, NULL, NULL };

void
ssa_free_id_hash (void)
{
  struct id_defined *id_el, *tmp;
  /* free identifiers in the hash table.  */
  HASH_ITER (hh, id_definitions, id_el, tmp)
    {
      HASH_DEL (id_definitions, id_el);
      free (id_el);
    }
}

/* Add new element to a table where we store variable versions for ssa
   replacement.  */
void 
ssa_register_new_var (tree var)
{
  char* s;
  struct id_defined *id_el = NULL;
  assert (TREE_CODE (var) == IDENTIFIER, "identifier expected");
  s = TREE_STRING_CST (TREE_ID_NAME (var));
  id_el = (struct id_defined*) malloc (sizeof (struct id_defined));
  /* Initialization.  */
  id_el->id = s;
#ifndef SSA
  id_el->counter = 0;
  id_el->counter_length = 1;
  id_el->divider = 10;
  /* if `id_new' is NULL, then variable wasn't yet redefined.  */
  id_el->id_new = NULL;
#endif
  HASH_ADD_KEYPTR (hh, id_definitions, 
		   id_el->id, strlen (id_el->id), id_el);
}

#ifndef SSA
void
ssa_reassign_var (struct id_defined *id_el,
		       tree lhs, tree vars, tree ext_vars)
{
/* We do the reassignment in the loop, because we don't want the 
   name of the new variable coincide with the existing one.
   We choose the first variable in the form of
   '<old_variable><some_number>' which a variable list lacks.  */
  do
    {
      /* Create a new string for variable.  */
      char* new_name = (char*) malloc (sizeof (char) 
				    * (id_el->counter_length 
				    + strlen (id_el->id) + 1));
      sprintf (new_name, "%s%d", id_el->id, id_el->counter);
      free (TREE_STRING_CST (TREE_ID_NAME (lhs)));
      
      TREE_STRING_CST (TREE_ID_NAME (lhs)) = new_name;

      /* Update the relevant entry in the hash table.  */
      id_el->id_new = new_name;
      if (!(++(id_el->counter) % id_el->divider))
	{
	  id_el->counter_length++;
	  id_el->divider *= 10;
	}
    } while (is_var_in_list (lhs, vars)
	  || is_var_in_list (lhs, ext_vars));

  tree_list_append (ext_vars, lhs);
}

tree
ssa_create_phi_node (basic_block bb, tree node)
{
  edge e1, e2;
  assert (TREE_CODE (node) == IDENTIFIER, "phi node can be created "
    "only from identifier");

  e1 = *((edge*) utarray_eltptr (bb->preds, 0));
  e2 = *((edge*) utarray_eltptr (bb->preds, 1));
  if (e1 != NULL && e2 != NULL)
    {
      struct id_defined_tree *el1 = NULL, *el2 = NULL;
      HASH_FIND_STR (e1->var_list,
		     TREE_STRING_CST (TREE_ID_SOURCE_NAME (node)), el1);
      HASH_FIND_STR (e2->var_list, 
		     TREE_STRING_CST (TREE_ID_SOURCE_NAME (node)), el2);
      if (el1 != NULL && el2 != NULL)
	{
	  tree phi = make_tree (PHI_NODE);
	  TREE_OPERAND_SET (phi, 0, el1->var);
	  TREE_OPERAND_SET (phi, 1, el2->var);
	  return phi;
	}
    }
  return NULL;
}

tree
ssa_localize_phi_node (basic_block bb, tree node)
{
  enum tree_code code = TREE_CODE (node);
  struct tree_list_element *el;
  int i;

  if (code == IDENTIFIER)
    return ssa_create_phi_node (bb, node);
  else if (code == LIST)
    {
      DL_FOREACH (TREE_LIST (node), el)
	{
	  tree phi = ssa_localize_phi_node (bb, el->entry);
	  if (phi != NULL)
	    el->entry = phi;
	}
    }
  else if (code == ASSIGN_STMT)
    {
      tree phi = ssa_localize_phi_node (bb, TREE_OPERAND (node, 1));
      if (phi != NULL)
	TREE_OPERAND_SET (node, 1, phi);
    }
  else if (code != DECLARE_STMT)
    {
      for (i = 0; i < TREE_CODE_OPERANDS (code); i++)
	{
	  tree phi = ssa_localize_phi_node (bb, TREE_OPERAND (node, i));
	  if (phi != NULL)
	    TREE_OPERAND_SET (node, i, phi);
	}
    }
  return NULL;
}
#else
void
ssa_hash_add_var (tree var)
{
  struct id_defined *id_el = (struct id_defined*) 
			     malloc (sizeof (struct id_defined));
  id_el->id = TREE_STRING_CST (TREE_ID_NAME (var));
  HASH_ADD_KEYPTR (hh, id_definitions, id_el->id, strlen (id_el->id), id_el);
}
#endif

