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

/* Copy information about variable instances.
   We call this function when we create a new block and variable information
   needs to be transfered between blocks.  */
struct id_defined*
ssa_copy_var_hash (struct id_defined *hash)
{
  struct id_defined *new_hash = NULL, *el, *tmp;
  HASH_ITER (hh, hash, el, tmp)
    {
      struct phi_node *hel, *htmp;
      struct id_defined* el_copy = (struct id_defined*) 
				   malloc (sizeof (struct id_defined));
      memcpy (el_copy, el, sizeof (struct id_defined));
      memset (&(el_copy->hh), 0, sizeof (UT_hash_handle));
      el_copy->id = el->id;
      if (el->id_new != NULL)
	el_copy->id_new = strdup (el->id_new);
      el_copy->phi_node = NULL;
      HASH_ITER (hh, el->phi_node, hel, htmp)
	{
	  struct phi_node *phi_node = (struct phi_node *)
				      malloc (sizeof (struct phi_node));
	  memset (phi_node, 0, sizeof (struct phi_node));
	  phi_node->s = hel->s;
	  HASH_ADD_KEYPTR (hh, el_copy->phi_node, phi_node->s, 
			   strlen (phi_node->s), phi_node);
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
  assert (TREE_CODE (var) == IDENTIFIER, "identifier expected");
  s = TREE_STRING_CST (TREE_ID_NAME (var));
  id_el = (struct id_defined *) malloc (sizeof (struct id_defined)); 
  /* if `id_new' is NULL, then variable wasn't yet redefined.  */
  memset (id_el, 0, sizeof (struct id_defined));
  /* Initialization.  */
  id_el->id = s;
  id_el->counter = 0;
  id_el->counter_length = 1;
  id_el->divider = 10;
  HASH_ADD_KEYPTR (hh, bb->var_hash, 
		   id_el->id, strlen (id_el->id), id_el);
}

char*
ssa_reassign_var (basic_block bb, tree var)
{
  struct id_defined *id_el = NULL, *tmp = NULL;
  char* source_name = TREE_STRING_CST (TREE_ID_NAME (var));
  HASH_FIND_STR (bb->var_hash, source_name, id_el);
  char* new_name = NULL;
  struct phi_node *el, *phi_tmp;
  if (id_el != NULL)
    {
      /* We do the reassignment in the loop, because we don't want the 
	 name of the new variable coincide with the existing one.
	 We choose the first variable in the form of 
	 '<old_variable><some_number>'.  */
      do
	{
	  /* Create a new string for variable.  */
	  new_name = (char*) malloc (sizeof (char) * (id_el->counter_length
	    + strlen (id_el->id) + 1));
	  sprintf (new_name, "%s%d", id_el->id, id_el->counter);

	  /* Update the relevant entry in the hash table.  */
	  if (id_el->id_new)
	    {
	      free (id_el->id_new);
	      id_el->id_new = NULL;
	    }
	  id_el->id_new = new_name;
	  while (!(++id_el->counter % id_el->divider))
	    {
	      id_el->counter_length++;
	      id_el->divider *= 10;
	    }
	  HASH_FIND_STR (bb->var_hash, new_name, tmp);
	} while (tmp != NULL);
      HASH_FREE (hh, id_el->phi_node, el, phi_tmp);
    }
  else
    ssa_declare_new_var (bb, var);
  return new_name;
}

#define replace_id_str(id, new) \
do \
  { \
    free (TREE_STRING_CST (TREE_ID_NAME (id))); \
    TREE_STRING_CST (TREE_ID_NAME (id)) = strdup (new); \
  } while (0);
/* Search subtree for a identifier node. If found, substitute this with a
   redefined identifier.  */
void
ssa_redefine_vars (basic_block bb, tree node)
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
	      char* name = ssa_reassign_var (bb, el->entry);
	      if (name != NULL)
		{
		  el->entry = tree_copy (el->entry);
		  replace_id_str (el->entry, name);
		}
	    }
	}
    }
  else if (code == ASSIGN_STMT)
    {
      if (TREE_CODE (TREE_OPERAND (node, 0)) == IDENTIFIER)
	{
	  char* name = ssa_reassign_var (bb, TREE_OPERAND (node, 0));
	  if (name != NULL)
	    {
	      TREE_OPERAND_SET (node, 0, tree_copy (TREE_OPERAND (node, 0)));
	      replace_id_str (TREE_OPERAND (node, 0), name);
	    }
	}
    }
  else
    {
      for (i = 0; i < TREE_CODE_OPERANDS (code); i++)
	{
	  if (TREE_CODE (TREE_OPERAND (node, i)) == IDENTIFIER)
	    {
	      char* name = ssa_reassign_var (bb, TREE_OPERAND (node, i));
	      if (name != NULL)
		{
		  TREE_OPERAND_SET (node, i, tree_copy (TREE_OPERAND (node, i)));
		  replace_id_str (TREE_OPERAND (node, i), name);
		}
	    }
	}
    }
}

/* Callback to sort strings inside utarray.  */
static int string_sort(struct phi_node *a, struct phi_node *b) 
{
    return strcmp (a->s, b->s);
}

void
ssa_verify_vars (basic_block bb, tree node)
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
		  /* Check for necessity of phi nodes.  */
		  if (id_el->phi_node && HASH_COUNT (id_el->phi_node) > 1)
		    {
		      struct phi_node *hel, *tmp;
		      tree new_node  = make_tree (PHI_NODE);
		      HASH_SORT (id_el->phi_node, string_sort);
		      HASH_ITER (hh, id_el->phi_node, hel, tmp)
			{
			  struct phi_node_tree *hash_tree_el = 
			    (struct phi_node_tree *) 
			     malloc (sizeof (struct phi_node_tree));
			  memset (hash_tree_el, 0, 
				  sizeof (struct phi_node_tree));
			  tree ttmp = tree_copy (el->entry);
			  replace_id_str (ttmp, hel->s);
			  hash_tree_el->s = hel->s;
			  hash_tree_el->node = ttmp;
			  HASH_ADD_KEYPTR (hh, TREE_PHI_NODE (new_node),
					   hash_tree_el->s,
					   strlen (hash_tree_el->s),
					   hash_tree_el);

			}
		      TREE_TYPE (new_node) = TREE_TYPE (el->entry);
		      tree_list_append (delete_list, el->entry);
		      el->entry = new_node;
		      /* Clear hash.  */
		      HASH_FREE (hh, id_el->phi_node, hel, tmp);
		    }
		  else if (id_el->id_new != NULL 
		      && strcmp (id_el->id_new, 
			       TREE_STRING_CST (TREE_ID_NAME (el->entry))))
		    replace_id_str (el->entry, id_el->id_new);
		}
	    }
	  else
	    ssa_verify_vars (bb, el->entry);
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
	      /* Check for necessity of phi nodes.  */
	      if (id_el->phi_node && HASH_COUNT (id_el->phi_node) > 1)
		{
		  struct phi_node *hel, *tmp;
		  tree new_node  = make_tree (PHI_NODE);
		  HASH_SORT (id_el->phi_node, string_sort);
		  HASH_ITER (hh, id_el->phi_node, hel, tmp)
		    {
		      struct phi_node_tree *hash_tree_el = 
			(struct phi_node_tree *) 
			 malloc (sizeof (struct phi_node_tree));
		      memset (hash_tree_el, 0, 
			      sizeof (struct phi_node_tree));
		      tree ttmp = tree_copy (TREE_OPERAND (node, 1));
		      replace_id_str (ttmp, hel->s);
		      hash_tree_el->s = hel->s;
		      hash_tree_el->node = ttmp;
		      HASH_ADD_KEYPTR (hh, TREE_PHI_NODE (new_node),
				       hash_tree_el->s,
				       strlen (hash_tree_el->s),
				       hash_tree_el);
		    }
		  tree_list_append (delete_list, TREE_OPERAND (node, 1));
		  TREE_OPERAND_SET (node, 1, new_node);
		  HASH_FREE (hh, id_el->phi_node, hel, tmp);
		}
	      else if (id_el->id_new != NULL 
		  && strcmp (id_el->id_new,
			TREE_STRING_CST (TREE_ID_NAME (TREE_OPERAND (node, 1)))))
		replace_id_str (TREE_OPERAND (node, 1), id_el->id_new);
	    }
	}
      else
	ssa_verify_vars (bb, TREE_OPERAND (node, 1));
      ssa_redefine_vars (bb, TREE_OPERAND (node, 0));
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
		  /* Check for necessity of phi nodes.  */
		  if (id_el->phi_node && HASH_COUNT (id_el->phi_node) > 1)
		    {
		      struct phi_node *hel, *tmp;
		      tree new_node  = make_tree (PHI_NODE);
		      HASH_SORT (id_el->phi_node, string_sort);
		      HASH_ITER (hh, id_el->phi_node, hel, tmp)
			{
			  struct phi_node_tree *hash_tree_el = 
			    (struct phi_node_tree *) 
			     malloc (sizeof (struct phi_node_tree));
			  memset (hash_tree_el, 0,
				  sizeof (struct phi_node_tree));
			  tree ttmp = tree_copy (TREE_OPERAND (node, i));
			  replace_id_str (ttmp, hel->s);
			  hash_tree_el->s = hel->s;
			  hash_tree_el->node = ttmp;
			  HASH_ADD_KEYPTR (hh, TREE_PHI_NODE (new_node),
					   hash_tree_el->s,
					   strlen (hash_tree_el->s),
					   hash_tree_el);
			}
		      tree_list_append (delete_list, TREE_OPERAND (node, i));
		      TREE_OPERAND_SET (node, i, new_node);
		      HASH_FREE (hh, id_el->phi_node, hel, tmp);
		    }
		  else if (id_el->id_new != NULL 
		      && strcmp (id_el->id_new,
			    TREE_STRING_CST (TREE_ID_NAME (TREE_OPERAND (node, i)))))
		    replace_id_str (TREE_OPERAND (node, i), id_el->id_new);
		}
	    }
	  else
	    ssa_verify_vars (bb, TREE_OPERAND (node, i));
	}
    }
}
