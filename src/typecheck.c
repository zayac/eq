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
#include "expand.h"
#include "tree.h"
#include "global.h"
#include "typecheck.h"
#include "print.h"
#include "types.h"

int
typecheck ()
{
  struct tree_list_element *tl;
  int function_check = 0;

  DL_FOREACH (TREE_LIST (function_list), tl)
    function_check += typecheck_function (tl->entry);

  return function_check;
}

int
typecheck_stmt_list (tree stmt_list, tree ext_vars, tree vars)
{
  struct tree_list_element *tle;
  int ret = 0;

  assert (TREE_CODE (stmt_list) == LIST, "statement list expected");

  DL_FOREACH (TREE_LIST (stmt_list), tle)
    ret += typecheck_stmt (tle->entry, ext_vars, vars);
  return ret;
}

static inline tree
is_var_in_list (tree var, tree lst)
{
  struct tree_list_element *tle;
  tree ret = NULL;
  assert (TREE_CODE (lst) == LIST, "Variable list expected");
  assert (TREE_CODE (var) == IDENTIFIER, "Variable expected");

  if (lst == NULL)
    return NULL;

  /* NOTE we *must* return the last match value  at this point
     because the list could be concatenation of two variable
     lists of nested blocks. */
  DL_FOREACH (TREE_LIST (lst), tle)
  {
    if (strcmp
	(TREE_STRING_CST (TREE_ID_NAME (var)),
	 TREE_STRING_CST (TREE_ID_NAME (tle->entry))) == 0)
      ret = tle->entry;
  }

  return ret;
}

int
typecheck_stmt (tree stmt, tree ext_vars, tree vars)
{
  int ret = 0;

  switch (TREE_CODE (stmt))
    {
    case ASSIGN_EXPR:
      {
	tree lhs, rhs, var;
	int rhs_ret;

	lhs = TREE_OPERAND (stmt, 0);
	rhs = TREE_OPERAND (stmt, 1);

	assert (TREE_CODE (lhs) == IDENTIFIER,
		"Left hand side of the assignment expression "
		"must be an identifier");

	if ((rhs_ret = typecheck_expression (rhs, vars, ext_vars)) != 0)
	  return rhs_ret;

	/* Check if the variable was defined locally.  */
	if ((var = is_var_in_list (lhs, vars)) != NULL
	    || (var = is_var_in_list (lhs, ext_vars)) != NULL)
	  {
	    /* Replace the variable with the variable from the list.  */
	    TREE_OPERAND_SET (stmt, 0, var);
	    free_tree (lhs);
	    lhs = TREE_OPERAND (stmt, 0);
	  }
	else
	  {
	    tree_list_append (ext_vars, lhs);
	  }

	/* FIXME Check that these types are not NULL.  */
	if (TREE_TYPE (lhs) == NULL)
	  TREE_TYPE (lhs) = TREE_TYPE (rhs);
	else if (!tree_compare (TREE_TYPE(lhs), TREE_TYPE(rhs)))
	  {
	    error_loc (TREE_LOCATION (lhs),
		       "Assignment left hand side type does not "
		       "match right hand side type");
	    return 1;
	  }
	return 0;
      }
      break;
    case DECLARE_EXPR:
      {
	tree lhs = TREE_OPERAND (stmt, 0);
	tree rhs = TREE_OPERAND (stmt, 1);
	tree var;

	assert (TREE_CODE (lhs) == IDENTIFIER,
		"Left hand side of the assignment expression "
		"must be an identifier");
	
	if (TREE_CODE_CLASS (TREE_CODE (rhs)) != tcl_type)
	  {
	    error_loc (TREE_LOCATION (rhs), "expected type, '%s' found",
				  TREE_CODE_NAME (TREE_CODE (rhs)));
	    return 1;
	  }

	if ((var = is_var_in_list (lhs, vars)) != NULL
	    || (var = is_var_in_list (lhs, ext_vars)) != NULL)
	  {
	    error_loc (TREE_LOCATION (lhs), "'%s' is already declared",
			TREE_STRING_CST (TREE_ID_NAME (var)));   
	    return 1;
	  }
	
	TREE_TYPE (lhs) = rhs;
	tree_list_append (vars, lhs);
      }
      break;
    default:
      assert (TREE_CODE_CLASS (TREE_CODE (stmt)) == tcl_expression,
	      "expression expected");
      //return typecheck_expression (stmt, ext_vars, vars);
    }

  return ret;
}

int
typecheck_function (tree func)
{
  struct tree_list_element *el, *type;
  tree vars;
  unsigned type_counter = 0, var_counter = 0;
  assert (TREE_CODE (func) == FUNCTION, "function tree expected");
  
  /* get argument names.  */
  vars = TREE_OPERAND (func, 1);
  /* get argument types.  */
  if (TREE_OPERAND (func, 2) != NULL)
    type = TREE_LIST (TREE_OPERAND (func, 2));
  else
    type = NULL;
  
  /* Check and assign types to arguments.  */
  if (vars != NULL)
    DL_FOREACH (TREE_LIST (vars), el)
      {
	/* check for identifier duplicates in argument list.  */
	tree dup = is_var_in_list (el->entry, vars);
	if (dup != NULL && dup != el->entry)
	  {
	    error_loc (TREE_LOCATION (func), "argument '%s' occures more "
			  "than once in argument list", 
			  TREE_STRING_CST (TREE_ID_NAME (dup)));
	    return 1;
	  }

	var_counter++;
	if (type == NULL)
	  {
	    /* count total argument number.  */
	    while (el->next != NULL)
	      {
		var_counter++;
		el = el->next;
	      }
	    error_loc (TREE_LOCATION (func), 
			"number of argument types is wrong: "
			"%u expected, %u found", var_counter, type_counter);
	    return 2;
	  }
	type_counter++;
	TREE_TYPE(el->entry) = type->entry;
	type = type->next;
      }
  if (type != NULL)
    {
      /* count total argument type number.  */
      while (type != NULL)
	{
	  type_counter++;
	  type = type->next;
	}
      error_loc (TREE_LOCATION (func), "number of argument types is wrong: "
		      "%u expected, %u found", var_counter, type_counter);
      return 2;
    }

  return typecheck_stmt_list (TREE_OPERAND (func, 4), vars, make_tree_list ());
}

int
typecheck_expression (tree expr, tree ext_vars, tree vars)
{
  int ret = 0;

  switch (TREE_CODE (expr))
    {
    case INTEGER_CST:
      TREE_TYPE (expr) = z_type_node;
      TREE_CONSTANT (expr) = true;
      break;
    case REAL_CST:
      TREE_TYPE (expr) = r_type_node;
      TREE_CONSTANT (expr) = true;
      break;
    case STRING_CST:
      TREE_TYPE (expr) = types_assign_type (STRING_TYPE, 
			    strlen(TREE_STRING_CST (expr)) + 1, NULL, NULL);
      TREE_CONSTANT (expr) = true;
      break;
    case IDENTIFIER:
      {
	tree var;

	/* The order of checking *is* important.  */
	if ((var = is_var_in_list (expr, vars)) != NULL
	    || (var = is_var_in_list (expr, ext_vars)) != NULL)
	  {
	    assert (TREE_TYPE (var) != NULL, 0);
	    TREE_TYPE (expr) = TREE_TYPE (var);
	  }
	else
	  {
	    error_loc (TREE_LOCATION (expr),
		       "Variable `%s' used without previous definition",
		       TREE_STRING_CST (TREE_ID_NAME (expr)));
	    ret = 1;
	  }
      }
      break;
    case PLUS_EXPR:
    case MINUS_EXPR:
    case DIV_EXPR:
    case MULT_EXPR:
    case MOD_EXPR:
      {
	tree lhs = TREE_OPERAND (expr, 0);
	tree rhs = TREE_OPERAND (expr, 1);

	ret += typecheck_expression (lhs, ext_vars, vars);
	ret += typecheck_expression (rhs, ext_vars, vars);

	if (ret != 0 || TREE_TYPE (lhs) == NULL || TREE_TYPE (rhs) == NULL)
	  return 1;

	if (!tree_compare (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	  {
	    error_loc (TREE_LOCATION (lhs), "type mismatch. the left operand "
					    "is %s, the right operand is %s",
		      TREE_CODE_NAME (TREE_CODE (TREE_TYPE (lhs))),
		      TREE_CODE_NAME (TREE_CODE (TREE_TYPE (rhs))));
	    return 1;
	  }
	else
	  TREE_TYPE (expr) = TREE_TYPE (lhs);
      }
      break;

    default:
      error ("cannot typecheck expression of type `%s'",
	     TREE_CODE_NAME (TREE_CODE (expr)));
      unreachable (0);
    }

  return ret;
}
