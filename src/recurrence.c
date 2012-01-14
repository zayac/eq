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

/* given the block of recurrent expression we are to check
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
  DL_FOREACH (TREE_LIST (t), el)
    {
      tree lhs = TREE_OPERAND (el->entry, 0);
      if (TREE_CODE (lhs) != INTEGER_CST)
	{
	  if (TREE_CODE (lhs) == MINUS_EXPR)
	    window[-min - TREE_INTEGER_CST (TREE_OPERAND (lhs, 1))] = true; 
	  else if (TREE_CODE (lhs) == PLUS_EXPR)
	    window[-min - TREE_INTEGER_CST (TREE_OPERAND (lhs, 1))] = true;
	  else
	    /* [\iter] case. */
	    window[max - min] = true;
	}
    }
  for (i = 0; i < window_size; i++)
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
