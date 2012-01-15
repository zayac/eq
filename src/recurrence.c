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

#include <stdio.h>
#include "eq.h"
#include "tree.h"
#include "global.h"
#include "print.h"
#include "types.h"
#include "recurrence.h"

/* find the biggest or the least shif from current iterator in recurrence
   block.
   p == 0 -- find the least;
   p == 1 -- find the biggest.  */
int
recurrence_find (tree t, bool p)
{
  struct tree_list_element *el;
  int ret;
  bool init = false;
  DL_FOREACH (TREE_LIST (t), el)
    {
      tree lhs = TREE_OPERAND (el->entry, 0);
      if (!p)
	{
	  if (TREE_CODE (lhs) == MINUS_EXPR
	   && (!init || -TREE_INTEGER_CST (TREE_OPERAND (lhs, 1)) < ret))
	    ret = -TREE_INTEGER_CST (TREE_OPERAND (lhs, 1));
	  else if (TREE_CODE (lhs) == PLUS_EXPR
	   && (!init || TREE_INTEGER_CST (TREE_OPERAND (lhs, 1)) < ret))
	    ret = TREE_INTEGER_CST (TREE_OPERAND (lhs, 1));
	  else if (TREE_CODE (lhs) == IDENTIFIER
	   && (!init || 0 < ret))
	    ret = 0;
	}
      else
	{
	  if (TREE_CODE (lhs) == MINUS_EXPR
	   && (!init || -TREE_INTEGER_CST (TREE_OPERAND (lhs, 1)) > ret))
	    ret = -TREE_INTEGER_CST (TREE_OPERAND (lhs, 1));
	  else if (TREE_CODE (lhs) == PLUS_EXPR
	   && (!init || TREE_INTEGER_CST (TREE_OPERAND (lhs, 1)) > ret))
	    ret = TREE_INTEGER_CST (TREE_OPERAND (lhs, 1));
	  else if (TREE_CODE (lhs) == IDENTIFIER
	   && (!init || 0 > ret))
	    ret = 0;
	}
      if (TREE_CODE (lhs) == MINUS_EXPR 
	|| TREE_CODE (lhs) == PLUS_EXPR
	|| TREE_CODE (lhs) == IDENTIFIER)
	init = true;
    }
  return ret;
}

/* Given the block of recurrent expression we are to check
   that all variables in relevant range are listed,
   i.e. if a^{[\iter - 5]} and a^{[\iter + 3]} are defined,
   make sure that all variables in range 
   [\iter - 5, ... \iter + 3] are listed.  */
int
recurrence_check_window (tree var)
{
  struct tree_list_element *el;
  tree t = TREE_ID_ITER (var);
  int min = recurrence_find_min (t), max = recurrence_find_max (t);
  size_t window_size = max - min + 1;
  bool window[window_size];
  int i;
  int ret = 0;
  memset (window, 0, window_size);
  bool init = false;
  DL_FOREACH (TREE_LIST (t), el)
    {
      tree lhs = TREE_OPERAND (el->entry, 0);
      if (TREE_CODE (lhs) != INTEGER_CST)
	{
	  if (TREE_CODE (lhs) == MINUS_EXPR)
	    window[-min - TREE_INTEGER_CST (TREE_OPERAND (lhs, 1))] = true; 
	  else if (TREE_CODE (lhs) == PLUS_EXPR)
	    window[-min + TREE_INTEGER_CST (TREE_OPERAND (lhs, 1))] = true; 
	  else
	    /* [\iter] case. */
	    window[-min] = true;
	  init = true;
	}
    }
  for (i = 0; i < window_size && init; i++)
    {
      if (!window[i])
	{
	  error ("value `\\iter + %d' is not set for variable `%s'",
	  min + i,
	  TREE_STRING_CST (TREE_ID_NAME (var)));
	  ret++;
	}
    }
  return ret;
}

/* this one is used for comparing indexes in both situations:
   either index is a constant ([5]) 
   or a recurrent expression ([\iter + 5]).  */
static int
check_recurrent_expression (tree t, int left_index, bool is_const)
{
  int i, ret = 0;
  if (TREE_CODE (t) == CIRCUMFLEX && TREE_CIRCUMFLEX_INDEX_STATUS (t))
    {
      tree index = TREE_OPERAND (t, 1);
      int right_index;
      if (!is_const && TREE_CODE (index) == MINUS_EXPR)
	right_index = -TREE_INTEGER_CST (TREE_OPERAND (index, 1));
      else if (!is_const && TREE_CODE (index) == PLUS_EXPR)
	right_index = TREE_INTEGER_CST (TREE_OPERAND (index, 1));
      else if (!is_const && TREE_CODE (index) == IDENTIFIER)
	right_index = 0;
      else if (is_const && TREE_CODE (index) == INTEGER_CST)
	right_index = TREE_INTEGER_CST (TREE_OPERAND (index, 1));
      else
	assert (0, "unexprected tree node `%s'", 
		  TREE_CODE_NAME (TREE_CODE (index)));

      if (left_index <= right_index)
	{
	  error_loc (TREE_LOCATION (TREE_OPERAND (t, 0)), "index in the right part "
	    "of the assignment must be lower than one in the left part");
	}
      ret += 1;
    }
  for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE (t)); i++)
    ret += check_recurrent_expression (TREE_OPERAND (t, i), 
	      left_index, is_const);
  return ret;
}

/* All indexes in the right part of assignment must precede the index in the
   left part of assignment. 
   NOTE May be this validation is needless.  */
int
recurrence_check_precedence (tree var)
{
  struct tree_list_element *el;
  tree t = TREE_ID_ITER  (var);
  int ret = 0;
  DL_FOREACH (TREE_LIST (t), el)
    {
      int left_index;
      tree lhs = TREE_OPERAND (el->entry, 0);
      if (TREE_CODE (lhs) == INTEGER_CST)
	{
	  left_index = TREE_INTEGER_CST (lhs);
	  ret += check_recurrent_expression (TREE_OPERAND (el->entry, 1),
		    left_index, true);
	}
      else
	{
	  if (TREE_CODE (lhs) == MINUS_EXPR)
	    left_index = -TREE_INTEGER_CST (TREE_OPERAND (lhs, 1));
	  else if (TREE_CODE (lhs) == PLUS_EXPR)
	    left_index = TREE_INTEGER_CST (TREE_OPERAND (lhs, 1));
	  else if (TREE_CODE (lhs) == IDENTIFIER)
	    left_index = 0;
	  else
	    assert (0, "unexprected tree node `%s'", 
		TREE_CODE_NAME (TREE_CODE (lhs)));
	  ret += check_recurrent_expression (TREE_OPERAND (el->entry, 1),
		    left_index, false);
	}
    }
  return ret;
}
