/* Copyright (c) 2011,2012 Artem Shinkarov <artyom.shinkaroff@gmail.com>
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

/* this function is used to sort definitions of a recurrent expressions.
   Passed as an argument to DL_SORT macros.  */
int
recurrence_sort (struct tree_list_element *l, struct tree_list_element *r)
{
  tree lel = TREE_OPERAND (l->entry, 0);
  tree rel = TREE_OPERAND (r->entry, 0);

  assert ((lel == iter_var_node
       || TREE_CODE (lel) == INTEGER_CST)
       && (rel == iter_var_node
       || TREE_CODE (rel) == INTEGER_CST),
       "unexpected type");

  if (lel == iter_var_node && rel == iter_var_node)
    return 0;
  if (lel == iter_var_node)
    return 1;
  if (rel == iter_var_node)
    return -1;
  /* both are integers.  */
  else
    {
      int lnum = TREE_INTEGER_CST (lel);
      int rnum = TREE_INTEGER_CST (rel);
      if (lnum < rnum)
	return -1;
      if (lnum == rnum)
	return 0;
      return 1;
    }
}

/* check the right part or the expression, which has to have indexes that
   are lower than index in the left part.  */
int
recurrence_check_relation (tree t, tree left)
{
  int i, ret = 0;
  if (TREE_CODE (t) == CIRCUMFLEX && TREE_CIRCUMFLEX_INDEX_STATUS (t))
    {
      if (TREE_CODE (left) == INTEGER_CST)
	{
	  tree index = TREE_OPERAND (t, 1);
	  if (TREE_CODE (index) == INTEGER_CST)
	    {
	      if (TREE_INTEGER_CST (left) <= TREE_INTEGER_CST (index))
		{
		  error_loc (TREE_LOCATION (index),
		  "index `%d' in the right part of recurrent expression is "
		  "greater or equal than index `%d' in the left part",
		  TREE_INTEGER_CST (index),
		  TREE_INTEGER_CST (left));
		  ret += 1;
		}
	    }
	  else 
	    {
	      char* s = tree_to_str (index);
	      error_loc (TREE_LOCATION (index), 
		"constant expression is expected"
		" in the right part of recurrent expression, `%s' found",
		s);
		ret += 1;
	      free (s);
	    }
	}
    }
  if (TREE_CODE (t) == LIST)
    {
      struct tree_list_element *el;
      DL_FOREACH (TREE_LIST (t), el)
	ret += recurrence_check_relation (el->entry, left);
    }

  for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE (t)); i++)
    {
      ret += recurrence_check_relation (TREE_OPERAND (t, i), left); 
    }
  return ret;

}

bool
recurrence_is_constant_expression (tree expr)
{
  int i;
  if (expr == iter_var_node)
    return false;
  for(i = 0; i < TREE_CODE_OPERANDS (TREE_CODE (expr)); i++)
    {
      if (!recurrence_is_constant_expression (TREE_OPERAND (expr, i)))
	return false; 
    }
  return true;
}

int 
recurrence_find_max_shift (tree t)
{
  int i, max = 0;
  if (TREE_CODE (t) == CIRCUMFLEX && TREE_CIRCUMFLEX_INDEX_STATUS (t))
    {
      tree index = TREE_OPERAND (t, 1);
      if (TREE_CODE (index) == MINUS_EXPR)
	{
	  if (TREE_INTEGER_CST (TREE_OPERAND (index, 1)) > max)
	    max = TREE_INTEGER_CST (TREE_OPERAND (index, 1));
	}
    }
  if (TREE_CODE (t) == LIST)
    {
      struct tree_list_element *el;
      DL_FOREACH (TREE_LIST (t), el)
	{
	  int tmp = recurrence_find_max_shift (el->entry);
	  if (tmp > max)
	    max = tmp;
	}
    }
  for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE (t)); i++)
    {
      int tmp = recurrence_find_max_shift (TREE_OPERAND (t, i));
      if (tmp > max)
	max = tmp;
    }
  return max;
}

int
recurrence_validate_indexes (tree t, tree left, tree var, int min, int range)
{
  int ret = 0, i;
  if (TREE_CODE (t) == CIRCUMFLEX && TREE_CIRCUMFLEX_INDEX_STATUS (t) 
    && TREE_OPERAND (t, 0) == var)
    {
      tree index = TREE_OPERAND (t, 1);
      if (TREE_CODE (index) == INTEGER_CST && left == iter_var_node)
	{
	  int num = TREE_INTEGER_CST (index);
	  if (num < min || num > min + range)
	    {
	      char* s = tree_to_str (index);
	      error_loc (TREE_LOCATION (index), "index %s doesn't belong "
		"to the initial state",
		s);
	      free (s);
	      ret += 1;
	    }
	}
    }
  for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE (t)); i++)
    ret +=  recurrence_validate_indexes (TREE_OPERAND (t, i), left, var, min, range);
  return ret;
}

/* validate recurrent expression.
   the list with definitions *must* be sorted by index.  */
int
recurrence_validate (tree t)
{
  int ret = 0;
  struct tree_list_element *el;
  int max_shift = 0, min = 0;
  t = TREE_ID_ITER_DEF (t);

  DL_FOREACH (TREE_LIST (TREE_ITER_LIST (TREE_ID_ITER (t))), el)
    {
      tree lhs = TREE_OPERAND (el->entry, 0);
      tree rhs = TREE_OPERAND (el->entry, 1);
      
      if (lhs == iter_var_node)
	max_shift = recurrence_find_max_shift (rhs);

      ret += recurrence_check_relation (rhs, lhs);
    }
  
  if (max_shift != 0)
    {
      int i = 0;
      bool min_exists = false;

      if (TREE_CODE (TREE_OPERAND (TREE_LIST (
		TREE_ITER_LIST (TREE_ID_ITER (t)))->entry, 0))
	== INTEGER_CST)
	{
	  min = TREE_INTEGER_CST (TREE_OPERAND (TREE_LIST (
		  TREE_ITER_LIST (TREE_ID_ITER (t)))->entry, 0));
	  min_exists = true;
	}

      if (max_shift == 1 && !min_exists)
	{
	  error ("there has to be one initial value for variable `%s'",
	      TREE_STRING_CST (TREE_ID_SOURCE_NAME (t)));
	  ret += 1;
	  return ret;
	}

      DL_FOREACH (TREE_LIST (TREE_ITER_LIST (TREE_ID_ITER (t))), el)
	{
	  tree lhs = TREE_OPERAND (el->entry, 0);
	  if (TREE_CODE (lhs) == INTEGER_CST)
	    {
	      if (i++ >= max_shift)
		{
		  error ("too many initial values found for variable `%s'", 
		    TREE_STRING_CST (TREE_ID_SOURCE_NAME (t)));
		  ret += 1;
		  return ret;
		}
	      else if (min_exists
		    && ((el->next != NULL 
		    && TREE_CODE (TREE_OPERAND (el->next->entry, 0))
			== INTEGER_CST
		    && TREE_INTEGER_CST (TREE_OPERAND (el->next->entry, 0))
			!= TREE_INTEGER_CST (TREE_OPERAND (el->entry, 0)) + 1)
		    || ((el->next == NULL
		    || TREE_CODE (TREE_OPERAND (el->next->entry, 0)) != INTEGER_CST)
		    && i != max_shift)))
		{
		  error ("there have to be %d consequent initial values for "
		         "variable `%s'",
			 max_shift,
			 TREE_STRING_CST (TREE_ID_SOURCE_NAME (t)));
		  ret += 1;
		  return ret;
		}
	    }
	}
    }
  DL_FOREACH (TREE_LIST (TREE_ITER_LIST (TREE_ID_ITER (t))), el)
    {
      ret += recurrence_validate_indexes (TREE_OPERAND (el->entry, 1),
			       TREE_OPERAND (el->entry, 0),
			       t,
			       min,
			       max_shift);
    }

  TREE_ITER_MIN (TREE_ID_ITER (t)) = min;
  TREE_ITER_SIZE (TREE_ID_ITER (t)) = max_shift;
  return ret;
}
