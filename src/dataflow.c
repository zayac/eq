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
  
  assert (TREE_CODE_CLASS (TREE_CODE (stmt)) == tcl_statement,
	  "this function is compatible with statements only");

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
  if (TREE_CODE (stmt) == LIST)
    DL_FOREACH (TREE_LIST (stmt), el)
      dataflow_mark_redundant_code (el->entry);
  /* We get definition nodes from use-def chains check statements related to
     them.  */
  else if (TREE_STMT_DEFS (stmt) != NULL)
    DL_FOREACH (TREE_LIST (TREE_STMT_DEFS (stmt)), el)
      dataflow_mark_redundant_code (el->entry);
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
      assert (TREE_FUNC_RETURN (tl->entry) != NULL,
	      "there should be at least one return statement");
      DL_FOREACH (TREE_LIST (TREE_FUNC_RETURN (tl->entry)), tle)
	dataflow_mark_redundant_code (tle->entry);
      if (TREE_FUNC_PRINT (tl->entry) != NULL)
	DL_FOREACH (TREE_LIST (TREE_FUNC_PRINT (tl->entry)), tle)
	  dataflow_mark_redundant_code (tle->entry);

      DL_FOREACH (TREE_LIST (TREE_OPERAND (tl->entry, 4)), tle)
	{
	  /* Entry statements are combined into a separate list.  */
	  if (TREE_STMT_DEFS (tle->entry) == NULL
	   && !TREE_STMT_IS_REDUNDANT (tle->entry))
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
dataflow_get_use_list (tree node, tree queue)
{
  struct tree_list_element *el;

  assert (TREE_CODE_CLASS (TREE_CODE (node)) == tcl_statement,
	  "this function is compatible with statements only");

  if (TREE_STMT_USES (node) != NULL)
      DL_FOREACH (TREE_LIST (TREE_STMT_USES (node)), el)
	{
	  if (!TREE_STMT_IS_REDUNDANT (el->entry))
	    {
	      /* Decrease a number of link nodes.  */
	      TREE_STMT_DEF_NUMBER (el->entry)--;
	      /* When all previous nodes were proceeded, we add the 
		 statement into the table.  */
	      if (TREE_STMT_DEF_NUMBER (el->entry) == 0)
		tree_list_append (queue, el->entry);
	    }
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
		  TREE_STMT_DEF_NUMBER (el->entry)--;
		  if (TREE_STMT_DEF_NUMBER (el->entry) == 0)
		    tree_list_append (queue, el->entry);
		}
	    }
	}
      return;
    }

  /* A recursive descent rouinte.  */
  if (TREE_CODE (node) == LIST)
    DL_FOREACH (TREE_LIST (node), el)
      dataflow_get_use_list (el->entry, queue);
}

/* Schedule statements returning *a list of statement lists* in the result.
   Each list entry represents a set of statements which can be safely executed
   concurrently. BFS traversal is used for this.  */
tree
dataflow_schedule (tree entry_list)
{
  struct tree_list_element *el;
  tree return_list = make_tree_list ();
  tree queue = make_tree_list ();
  bool clean_entry_list = false;
  do
    {
      tree instr_list = make_tree_list ();
      DL_FOREACH (TREE_LIST (entry_list), el)
	{
	  tree_list_append (instr_list, el->entry);
	  dataflow_get_use_list (el->entry, queue);
	  printf ("%zd\n", TREE_LOCATION (el->entry).line);
	}
      printf ("---\n");
      tree_list_append (return_list,  instr_list);
      if (clean_entry_list)
	free_list (entry_list);
      entry_list = NULL;
      clean_entry_list = true;
      entry_list = queue;
      queue = make_tree_list ();
    } while (TREE_LIST (entry_list) != NULL);
  free (entry_list);
  free (queue);
  return return_list;  
}
