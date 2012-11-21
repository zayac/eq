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

#include <stdio.h>
#include "tree.h"
#include "global.h"
#include "ssa.h"
#include "dataflow.h"

struct tree_hash_node;

/* A statement is marked as redundant, if it's not used for calculating return
   value.
   Basically this marks *nonredundant* statements, because each node is 
   redundant by default.  */
void
dataflow_mark_redundant_code (tree stmt)
{
  struct tree_list_element *el;
  int i;
  if (stmt == NULL)
    return;

  if (TREE_CODE_CLASS (TREE_CODE (stmt)) == tcl_statement)
    {
      if (TREE_STMT_REDUNDANCE_CHECKED (stmt))
	return;
      TREE_STMT_REDUNDANCE_CHECKED (stmt) = true;
      /* If we reached this code, then a statement is not redundant.  */
      TREE_STMT_IS_REDUNDANT (stmt) = false;
      /* If statement is under an `if' statement scope,
	 then it's conditional expression is a dependency as well. */
      if (TREE_STMT_PARENT_IF (stmt) != NULL)
	dataflow_mark_redundant_code (TREE_STMT_PARENT_IF (stmt));
    }
  /* In case `if' statement we want to check just predicate, not inner blocks.
     Basically, this is because we checked them already if we reached this
     statement.  */
  if (TREE_CODE (stmt) == IF_STMT)
    {
      dataflow_mark_redundant_code (TREE_OPERAND (stmt, 0));
      return;
    }
  else if (TREE_CODE (stmt) == LIST)
    {
      DL_FOREACH (TREE_LIST (stmt), el)
	dataflow_mark_redundant_code (el->entry);
    }
  for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE (stmt)); i++)
    {
      dataflow_mark_redundant_code (TREE_OPERAND (stmt, i));
    }
  /* We get definition nodes from use-def chains check statements related to
     them.  */
  if (TREE_CODE (stmt) == IDENTIFIER && stmt != iter_var_node
   && TREE_ID_UD_CHAIN (stmt) != NULL)
    {
      DL_FOREACH (TREE_LIST (TREE_ID_UD_CHAIN (stmt)), el)
	dataflow_mark_redundant_code (TREE_ID_DEF (el->entry));
    }
}

/* We run this function to check either statement has `def' dependencies. If
   not, it can be used as an function entry statement.
   The first argument is a statement to check, and the second one is function
   argument list.  */
bool
dataflow_is_entry_point (tree node, tree args)
{
  struct tree_list_element *el;
  int i;
  if (node == NULL)
    return true;

  /* A redundant statement can be considered as entry point.  */
  if (TREE_CODE_CLASS (TREE_CODE (node)) == tcl_statement
   && TREE_STMT_IS_REDUNDANT (node))
    return false;

  if (TREE_CODE (node) == IDENTIFIER && node != iter_var_node)
    {
      /* This indicated that node does not have `def' dependencies.  */
      if (TREE_ID_UD_CHAIN (node) == NULL)
	return true;
      else
	return false;
    }
 
  /* In case if statement we need to validate conditional expression only.  */
  if (TREE_CODE (node) == IF_STMT)
    return dataflow_is_entry_point (TREE_OPERAND (node, 0), args);

  /* A recursive descent routing.  */
  if (TREE_CODE (node) == LIST)
    DL_FOREACH (TREE_LIST (node), el)
      {
	if (!dataflow_is_entry_point (el->entry, args))
	  return false;
      }
  for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE (node)); i++)
    if (!dataflow_is_entry_point (TREE_OPERAND (node, i), args))
      return false;

  return true;
}

int
dataflow (void)
{
  struct tree_list_element *tl, *tle;
  DL_FOREACH (TREE_LIST (function_list), tl)
    {
      /* We mark redundant statements to skip their processing in the future.

	 FIXME We need to remove from the AST and deallocate such statements in
	 the future.*/
      DL_FOREACH (TREE_LIST (TREE_FUNC_RETURN (tl->entry)), tle)
	dataflow_mark_redundant_code (tle->entry);

      DL_FOREACH (TREE_LIST (TREE_OPERAND (tl->entry, 4)), tle)
	{
	  /* Entry statements are combined into a separate list.  */
	  if (dataflow_is_entry_point (tle->entry, 
				       TREE_OPERAND (tl->entry, 1)))
	    {
	      if (TREE_FUNC_ENTRY (tl->entry) == NULL)
		TREE_FUNC_ENTRY (tl->entry) = make_tree_list ();
	      tree_list_append (TREE_FUNC_ENTRY (tl->entry),
				tle->entry);
	    }
	}
      TREE_FUNC_SCHEDULE (tl->entry) = 
		    dataflow_schedule (TREE_FUNC_ENTRY (tl->entry));
    }
  printf ("note: finished data flow analysis  [ok]\n");
  return 0;
}

/* Add `use' links of `node' statement definition into `hash' hash table.
   To satisfy all dependency links we add statement into table only when all
   previous statements were proceeded.  */
void
dataflow_get_use_list (tree node, struct tree_hash_node **hash)
{
  struct tree_list_element *el;
  int i;

  if (node == NULL)
    return;

  if (TREE_CODE (node) == ASSIGN_STMT)
    {
      dataflow_get_use_list (TREE_OPERAND (node, 0), hash);
      return;
    }
  
  /* Resolve control flow dependencies.  */
  if (TREE_CODE (node) == IF_STMT)
    {
      struct tree_list_element *el;
      int i;
      for (i = 1; i < 3; i++)
	{
	  tree op = TREE_OPERAND (node, i);
	  if (op == NULL)
	    break;
	  DL_FOREACH (TREE_LIST (TREE_OPERAND (node, i)), el)
	    {
	      if (!TREE_STMT_IS_REDUNDANT (el->entry))
		{
		  struct tree_hash_node *hash_el;
		  TREE_STMT_DEF_NUMBER (el->entry)--;
		  if (TREE_STMT_DEF_NUMBER (el->entry) == 0)
		    {
			   hash_el = (struct tree_hash_node *)
			    malloc (sizeof (struct tree_hash_node));
			  hash_el->s = el->entry;
			  HASH_ADD_PTR (*hash, s, hash_el);
		    }
		}
	    }
	}
      return;
    }

  /* Resolve data flow dependencies.  */
  if (TREE_CODE (node) == IDENTIFIER && node != iter_var_node)
    {
      if (TREE_ID_DU_CHAIN (node) != NULL)
	{
	  struct tree_list_element *el;
	  DL_FOREACH (TREE_LIST (TREE_ID_DU_CHAIN (node)), el)
	    {
	      if (!TREE_STMT_IS_REDUNDANT (TREE_ID_DEF (el->entry)))
		{
		  struct tree_hash_node *hash_el;
		  /* Decrease a number of link nodes.  */
		  TREE_STMT_DEF_NUMBER (TREE_ID_DEF (el->entry))--;
		  /* When all previous nodes were proceeded, we add the 
		     statement into the table.  */
		  if (TREE_STMT_DEF_NUMBER (TREE_ID_DEF (el->entry)) == 0)
		    {
		      hash_el = (struct tree_hash_node *)
			malloc (sizeof (struct tree_hash_node));
		      hash_el->s = TREE_ID_DEF (el->entry);
		      HASH_ADD_PTR (*hash, s, hash_el);
		    }
		}
	    }
	}
    }

  /* A recursive descent rouinte.  */
  if (TREE_CODE (node) == LIST)
    DL_FOREACH (TREE_LIST (node), el)
      dataflow_get_use_list (el->entry, hash);
  for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE (node)); i++)
    dataflow_get_use_list (TREE_OPERAND (node, i), hash);
}

/* Schedule statements returning *a list of statement lists* in the result.
   Each list entry represents a set of statements which can be safely executed
   concurrently. BFS traversal is used for this.  */
tree
dataflow_schedule (tree entry_list)
{
  struct tree_list_element *el;
  tree return_list = make_tree_list ();
  struct tree_hash_node *hash = NULL;
  bool clean_entry_list = false;
  /* `entry_list' is a FIFO queue for BFS algorithm.  */
  do
    {
      tree instr_list = make_tree_list ();
      struct tree_hash_node *hel, *tmp;
      DL_FOREACH (TREE_LIST (entry_list), el)
	{
	  tree_list_append (instr_list, el->entry);
	  dataflow_get_use_list (el->entry, &hash);
	  printf ("%zd\n", TREE_LOCATION (el->entry).line);
	}
      printf ("---\n");
      tree_list_append (return_list,  instr_list);
      if (clean_entry_list)
	free_list (entry_list);
      entry_list = NULL;
      clean_entry_list = true;
      HASH_ITER (hh, hash, hel, tmp)
	{
	  HASH_DEL (hash, hel);
	  if (entry_list == NULL)
	    entry_list = make_tree_list ();
	  tree_list_append (entry_list, hel->s);
	  free (hel);
	}
    } while (entry_list != NULL);
  return return_list;  
}
