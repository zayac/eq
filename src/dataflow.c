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
#include "dataflow.h"

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
      TREE_STMT_IS_REDUNDANT (stmt) = false;
      if (TREE_STMT_PARENT_IF (stmt) != NULL)
	dataflow_mark_redundant_code (TREE_STMT_PARENT_IF (stmt));
    }
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
  if (TREE_CODE (stmt) == IDENTIFIER && stmt != iter_var_node
   && TREE_ID_UD_CHAIN (stmt) != NULL)
    {
      DL_FOREACH (TREE_LIST (TREE_ID_UD_CHAIN (stmt)), el)
	dataflow_mark_redundant_code (TREE_ID_DEF (el->entry));
    }
}

int
dataflow (void)
{
  struct tree_list_element *tl, *tle;
  DL_FOREACH (TREE_LIST (function_list), tl)
    {
      DL_FOREACH (TREE_LIST (TREE_FUNC_RETURN (tl->entry)), tle)
	dataflow_mark_redundant_code (tle->entry);
      dataflow_function (tl->entry);
    }
  printf ("note: finished data flow analysis  [ok]\n");
  return 0;
}

tree
dataflow_function (tree func)
{
  struct tree_list_element *el;
  DL_FOREACH (TREE_LIST (TREE_FUNC_RETURN (func)), el)
    {
      //dataflow_chain (xf, el->entry);
    }
  return NULL;
}

#if 0
void
find_ids (tree expr, tree ids)
{
  struct tree_list_element *el;
  int i;
  if (TREE_CODE (expr) == IDENTIFIER)
    tree_list_append (ids, expr);
  if (TREE_CODE (expr) == LIST)
    DL_FOREACH (TREE_LIST (expr), el)
      find_ids (el->entry, ids);
  for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE (expr)); i++)
    find_ids (TREE_OPERAND (expr, i), ids);
}

void
print_link (xfile *xf, tree expr)
{
  struct tree_list_element *el, *hel;
  tree llist = make_tree_list ();
  tree rlist = make_tree_list ();
  find_ids (TREE_OPERAND (expr, 0), llist);
  find_ids (TREE_OPERAND (expr, 1), rlist);
  DL_FOREACH (TREE_LIST (rlist), el)
    {
      DL_FOREACH (TREE_LIST (TREE_ID_UD_CHAIN (el->entry)), hel)
	{
	  print_expression (xf, TREE_LIST (llist)->entry);
	  printf ("_%zd ->", TREE_LOCATION (TREE_LIST (llist)->entry).line);
	  print_expression (xf, el->entry);
	  printf ("_%zd, ", TREE_LOCATION (hel->entry).line);
	  print_link (xf, TREE_ID_DEF (hel->entry));
	}
    }
}

tree dataflow_chain (xfile *xf, tree expr)
{
  int i;
  struct tree_list_element *el;
  if (expr == NULL)
    return NULL;
  if (TREE_CODE (expr) == IDENTIFIER)
    {
      if (TREE_ID_UD_CHAIN (expr) != NULL)
	{
	  DL_FOREACH (TREE_LIST (TREE_ID_UD_CHAIN (expr)), el)
	    print_link (xf, TREE_ID_DEF (el->entry)); 
	}
    }
  if (TREE_CODE (expr) == LIST)
    {
      DL_FOREACH (TREE_LIST (expr), el)
	dataflow_chain (xf, el->entry);
    }
  for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE (expr)); i++)
    {
      dataflow_chain (xf, TREE_OPERAND (expr, i));
    }
  return NULL;
}
#endif
