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
  struct tree_hash_node *el, *phi_tmp;
  HASH_FIND_STR (bb->var_hash, source_name, id_el);
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

void
ssa_append_var_definition (basic_block bb, tree var)
{
  struct id_defined *id_el = NULL;
  char* source_name = TREE_STRING_CST (TREE_ID_NAME (var));
  struct tree_hash_node *phi_node;
  HASH_FIND_STR (bb->var_hash, source_name, id_el);
  /* id_el == NULL is the case when array is given as a function argument.  */
  if (id_el == NULL)
    ssa_reassign_var (bb, var);
  else
    {
      phi_node = (struct tree_hash_node *) malloc (sizeof (struct id_defined));
      id_el->was_modified = true;
      phi_node->s = var;
      HASH_ADD_PTR (id_el->phi_node, s, phi_node);
    }
}

/* Search subtree for a identifier node. If found, substitute this with a
   redefined identifier.  */
void
ssa_redefine_vars (basic_block bb, tree node, tree assign_node)
{
  enum tree_code code = TREE_CODE (node);
  struct tree_list_element *el;
  int i;

  if (code == LOWER)
    {
      assert (TREE_CODE (TREE_OPERAND (node, 0)) == IDENTIFIER,
	      "node is not an identifier");
      ssa_append_var_definition (bb, TREE_OPERAND (node, 0));
      TREE_ID_DEF (TREE_OPERAND (node, 0)) = assign_node;
      return;
    }
  else if (code == IDENTIFIER)
    {
      TREE_ID_DEF (node) = assign_node;
      ssa_reassign_var (bb, node);
    }
  else if (code == LIST)
    {
      DL_FOREACH (TREE_LIST (node), el)
	ssa_redefine_vars (bb, el->entry, assign_node);
    }
  for (i = 0; i < TREE_CODE_OPERANDS (code); i++)
    ssa_redefine_vars (bb, TREE_OPERAND (node, i), assign_node);
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
  enum tree_code code;
  struct tree_list_element *el;
  static bool link_deps = true;
  int i;

  if (node == NULL)
    return;

  code = TREE_CODE (node);

  if (code == LOWER)
    {
      bool tmp_link = link_deps;
      link_deps = true;
      ssa_verify_vars (bb, TREE_OPERAND (node, 0), stmt);
      ssa_verify_vars (bb, TREE_OPERAND (node, 1), stmt);
      link_deps = tmp_link;
      return;
    }
  else if (code == IDENTIFIER && link_deps)
    {
      struct id_defined *id_el = NULL;
      HASH_FIND_STR (bb->var_hash,
	  TREE_STRING_CST (TREE_ID_NAME (node)), id_el);
      if (id_el != NULL)
	{
	  struct tree_hash_node *hel, *tmp;
	  HASH_SORT (id_el->phi_node, pointer_sort);
	  /* Create use-def and def-use chain.  */
	  TREE_ID_UD_CHAIN (node) = make_tree_list ();
	  if (TREE_STMT_DEFS (stmt) == NULL)
	    TREE_STMT_DEFS (stmt) = make_tree_list ();
	  HASH_ITER (hh, id_el->phi_node, hel, tmp)
	    {
	      tree_list_append (TREE_ID_UD_CHAIN (node), hel->s);
	      tree_list_append (TREE_STMT_DEFS (stmt),
				TREE_ID_DEF (hel->s));
	      TREE_STMT_DEF_NUMBER (stmt)++;
	      if (TREE_ID_DU_CHAIN (hel->s) == NULL)
		TREE_ID_DU_CHAIN (hel->s) = make_tree_list ();
	      if (TREE_STMT_USES (TREE_ID_DEF (hel->s)) == NULL)
		TREE_STMT_USES (TREE_ID_DEF (hel->s)) = make_tree_list ();
	      tree_list_append (TREE_ID_DU_CHAIN (hel->s), 
				node);
	      tree_list_append (TREE_STMT_USES (TREE_ID_DEF (hel->s)),
				stmt);
	    }
	}
      TREE_ID_DEF (node) = stmt;
      return;
    }
  else if (code == LIST)
    DL_FOREACH (TREE_LIST (node), el)
      ssa_verify_vars (bb, el->entry, stmt);
  else if (code == ASSIGN_STMT)
    {
      ssa_verify_vars (bb, TREE_OPERAND (node, 1), stmt);
      link_deps = false;
      ssa_verify_vars (bb, TREE_OPERAND (node, 0), stmt);
      link_deps = true;
      ssa_redefine_vars (bb, TREE_OPERAND (node, 0), stmt);
      return;
    }
  else if (code == PARALLEL_LOOP_STMT)
    {
      ssa_verify_vars (bb, TREE_OPERAND (node, 1), stmt);
      ssa_verify_vars (bb, TREE_OPERAND (node, 2), stmt);
      link_deps = false;
      ssa_verify_vars (bb, TREE_OPERAND (node, 0), stmt);
      link_deps = true;
      ssa_redefine_vars (bb, TREE_OPERAND (node, 0), stmt);
      return;
    }

  for (i = 0; i < TREE_CODE_OPERANDS (code); i++)
    {
      if (code == IF_STMT && i > 0)
	break;
      ssa_verify_vars (bb, TREE_OPERAND (node, i), stmt);
    }
}
