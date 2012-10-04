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

#ifndef SSA
#include "ssa.h"

struct id_defined*
ssa_copy_var_hash (struct id_defined *hash)
{
  struct id_defined *new_hash = NULL, *el, *tmp;
  HASH_ITER (hh, hash, el, tmp)
    {
      struct id_defined* el_copy = (struct id_defined*) 
				   malloc (sizeof (struct id_defined));
      memcpy (el_copy, el, sizeof (struct id_defined));
      memset (&(el_copy->hh), 0, sizeof (UT_hash_handle));
      if (hash->id != NULL)
	el_copy->id = strdup (hash->id);
      if (hash->id_new != NULL)
	el_copy->id_new = strdup (hash->id_new);
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
  id_el = (struct id_defined*) malloc (sizeof (struct id_defined));
  /* Initialization.  */
  id_el->id = s;
  id_el->counter = 0;
  id_el->counter_length = 1;
  id_el->divider = 10;
  /* if `id_new' is NULL, then variable wasn't yet redefined.  */
  id_el->id_new = NULL;
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
  if (id_el != NULL)
    {
    /* We do the reassignment in the loop, because we don't want the 
       name of the new variable coincide with the existing one.
       We choose the first variable in the form of
       '<old_variable><some_number>' which a variable list lacks.  */
      do
	{
	  /* Create a new string for variable.  */
	  new_name = (char*) malloc (sizeof (char) * (id_el->counter_length				      + strlen (id_el->id) + 1));
	  sprintf (new_name, "%s%d", id_el->id, id_el->counter);

	  /* Update the relevant entry in the hash table.  */
	  id_el->id_new = new_name;
	  if (!(++(id_el->counter) % id_el->divider))
	    {
	      id_el->counter_length++;
	      id_el->divider *= 10;
	    }
	  HASH_FIND_STR (bb->var_hash, new_name, tmp);
	} while (tmp != NULL);
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
		  replace_id_str (TREE_OPERAND (node, 0), name);
		}
	    }
	}
    }

}

void
ssa_verify_vars (basic_block bb, tree node)
{
  enum tree_code code = TREE_CODE (node);
  struct tree_list_element *el;
  int i;
  if (code == LIST)
    {
      DL_FOREACH (TREE_LIST (node), el)
	{
	  if (TREE_CODE (el->entry) == IDENTIFIER)
	    {
	      struct id_defined *id_el = NULL;
	      HASH_FIND_STR (bb->var_hash, 
		  TREE_STRING_CST (TREE_ID_NAME (el->entry)), id_el);
	      if (id_el != NULL && id_el->id_new != NULL
	       && !strcmp (id_el->id_new, 
			   TREE_STRING_CST (TREE_ID_NAME(el->entry))))
		{
		    el->entry = tree_copy (el->entry);
		    replace_id_str (el->entry, id_el->id_new);
		}
	    }
	  else
	    ssa_verify_vars (bb, el->entry);
	  //if (TREE_CODE (el->entry) == IF_STMT)
	  //  break;
	}
    }
  else if (code == ASSIGN_STMT)
    {
      if (TREE_CODE (TREE_OPERAND (node, 1)) == IDENTIFIER)
	{
	  struct id_defined *id_el = NULL;
	  HASH_FIND_STR (bb->var_hash, 
	      TREE_STRING_CST (TREE_ID_NAME (node)), id_el);
	  if (id_el != NULL && id_el->id_new != NULL
	   && !strcmp (id_el->id_new, 
		       TREE_STRING_CST (TREE_ID_NAME(TREE_OPERAND (node, 1)))))
	    {
		TREE_OPERAND_SET (node, 1, tree_copy (TREE_OPERAND (node, 1)));
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
	 // if (code == IF_STMT && i > 0)
	 //   break;
	  if (TREE_CODE (TREE_OPERAND (node, i)) == IDENTIFIER)
	    {
	      struct id_defined *id_el = NULL;
	      HASH_FIND_STR (bb->var_hash, 
		  TREE_STRING_CST (TREE_ID_NAME (TREE_OPERAND (node, i))), id_el);
	      if (id_el != NULL && id_el->id_new != NULL
	       && !strcmp (id_el->id_new, 
		      TREE_STRING_CST (TREE_ID_NAME(TREE_OPERAND (node, i)))))
		{
		    TREE_OPERAND_SET (node, i, tree_copy (el->entry));
		    replace_id_str (TREE_OPERAND (node, i), id_el->id_new);
		}
	    }
	  else
	    ssa_verify_vars (bb, TREE_OPERAND (node, i));
	}
    }
}
#endif
