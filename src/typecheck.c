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
typecheck_stmt_list (tree stmt_list, tree ext_vars, tree vars, tree func)
{
  struct tree_list_element *tle;
  int ret = 0;

  assert (TREE_CODE (stmt_list) == LIST, "statement list expected");

  DL_FOREACH (TREE_LIST (stmt_list), tle)
    ret += typecheck_stmt (tle->entry, ext_vars, vars, func);
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

/* Check either type conversion is possible.
   NOTE types are not validated here.  */
bool
conversion_possible (tree from, tree to)
{
  if ((from == z_type_node 
    || from == n_type_node)
    && to == r_type_node)
    return true;
  
  if (TYPE_DIM (from) != NULL
   && TYPE_DIM (to) != NULL)
    {
      if (tree_compare (TYPE_DIM (from), TYPE_DIM (to)))
	{
	  if  ((tree_compare (TYPE_SHAPE (from), TYPE_SHAPE (to))
	   ||   TYPE_SHAPE (to) == NULL)
	   && ((TREE_CODE (from) == Z_TYPE || TREE_CODE (from) == N_TYPE)
	   &&  (TREE_CODE (to) == R_TYPE || TREE_CODE (to) == Z_TYPE
	      || TREE_CODE (to) == N_TYPE)))
	    return true;
	}
    }

  return false;
}

int 
typecheck_type (tree type, tree ext_vars, tree vars)
{
  int ret = 0;
  

  assert (TREE_CODE_CLASS ( TREE_CODE (type)) == tcl_type,
	  "type node must belong to class type");

  /* validate dimension.  */
  if (TYPE_DIM (type) != NULL)
    {
      int ret_val;
      if ((ret_val = typecheck_expression (TYPE_DIM (type), ext_vars, vars)))
	return ret_val;

      /* dimension must be some kind of integer type.
	 We check here just static values.  */
      if ((TREE_TYPE	(TYPE_DIM (type)) != z_type_node
	  && (TREE_TYPE (TYPE_DIM (type)) != n_type_node
		     ||  TYPE_DIM (type)  != NULL
		     || TYPE_SHAPE (type) != NULL)))
	{
	  error_loc (TREE_LOCATION (TYPE_DIM (type)), 
		    "type dimension must be an integer");
	  ret += 1;
	}
    }
   
  /* validate shape.  */
  if (TYPE_SHAPE (type) != NULL)
    {
      int dim = 0;
      struct tree_list_element *el;
      if (TYPE_DIM (type) != NULL && TREE_CONSTANT (TYPE_DIM (type)))
	dim = TREE_INTEGER_CST (TYPE_DIM (type));
      /* in general case, a shape is a list of integers.  */
      DL_FOREACH (TREE_LIST (TYPE_SHAPE(type)), el)
	{
	  int ret_val;
	  tree t = el->entry;
	  if (--dim <= -1)
	    goto shape_fail;
	  if ((ret_val = typecheck_expression (t, ext_vars, vars)))
	    return ret_val;
  
	  if (TREE_TYPE (t) != z_type_node
	     ||   TYPE_DIM (TREE_TYPE (t)) != NULL
	     || TYPE_SHAPE (TREE_TYPE (t)) != NULL)
	    {
	      error_loc (TREE_LOCATION (t),
		"type shape must be a list of integers");
	      ret += 1;
	    }
	}
      if (dim != 0)
	goto shape_fail;
    }

  return ret;

shape_fail:
  error_loc (TREE_LOCATION (TYPE_SHAPE (type)),
    "type shape doesn't correspond with the dimension %d",
    TREE_INTEGER_CST (TYPE_DIM (type)));
  return ret + 1;
}

int
typecheck_stmt (tree stmt, tree ext_vars, tree vars, tree func)
{
  int ret = 0;

  switch (TREE_CODE (stmt))
    {
    case ASSIGN_STMT:
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
	    /* try to convert types.  */
	    if (conversion_possible (TREE_TYPE (rhs), TREE_TYPE (lhs)))
	      {
		tree t = make_binary_op (CONVERT_EXPR, rhs, TREE_TYPE (lhs));
		TREE_OPERAND_SET (stmt, 1, t);
	      }
	    else
	      {
		error_loc (TREE_LOCATION (rhs),
		       "Assignment left hand side type does not "
		       "match right hand side type");
		return 1;
	      }
	  }
	return 0;
      }
      break;
    case DECLARE_STMT:
      {
	tree lhs = TREE_OPERAND (stmt, 0);
	tree rhs = TREE_OPERAND (stmt, 1);
	tree var;

	assert (TREE_CODE (lhs) == IDENTIFIER,
		"Left hand side of the assignment expression "
		"must be an identifier");
	
	if (TREE_CODE_CLASS (TREE_CODE (rhs)) != tcl_type)
	  {
	    error_loc (TREE_LOCATION (rhs), "expected type, `%s' found",
				  TREE_CODE_NAME (TREE_CODE (rhs)));
	    return 1;
	  }
	else
	  {
	    int ret_val;
	    if ((ret_val = typecheck_type (rhs, ext_vars, vars)))
	      return ret_val;
	    if (TREE_TYPE (rhs) == NULL)
	      return 1;
	  }
	
	if ((var = is_var_in_list (lhs, vars)) != NULL
	    || (var = is_var_in_list (lhs, ext_vars)) != NULL)
	  {
	    error_loc (TREE_LOCATION (lhs), "`%s' is already declared",
			TREE_STRING_CST (TREE_ID_NAME (var)));   
	    return 1;
	  }
	
	TREE_TYPE (lhs) = rhs;
	tree_list_append (vars, lhs);
      }
      break;
    case IF_STMT:
      {
	tree condition = TREE_OPERAND (stmt, 0);
	tree tr_stmts = TREE_OPERAND (stmt, 1);
	tree fs_stmts = TREE_OPERAND (stmt, 2);
	tree new_scope = NULL;

	/* Check condition type.  */
	ret += typecheck_expression (condition, ext_vars, vars);
	if (TREE_TYPE (condition) != b_type_node)
	  {
	    error_loc (TREE_LOCATION (condition),
	      "condition expression is not boolean");
	    ret += 1;
	  }
	
	/* A new scope of variables is opened here. That's why we combine
	   ext_vars and vars lists.  We just reassign pointers.  No additional
	   memory is allocated.  */
	tree_list_combine (ext_vars, vars);
	new_scope = make_tree_list ();
	ret += typecheck_stmt_list (tr_stmts, ext_vars, new_scope, func);
	free_tree (new_scope);
	/* Another scope for "else" statement list.  */
	if (fs_stmts != NULL)
	  {
	    new_scope = make_tree_list ();
	    ret += typecheck_stmt_list (fs_stmts, ext_vars, new_scope, func);
	    free_tree (new_scope);
	  }
	/* split combined lists back.  */
	tree_list_split (ext_vars, vars);
      }
      break;
    case RETURN_STMT:
      {
	ret += typecheck_expression (TREE_OPERAND (stmt, 0), ext_vars, vars);
	if (!tree_compare (TREE_TYPE (TREE_OPERAND (stmt, 0)), 
	      TREE_OPERAND (func, 3)))
	  {
	    if (conversion_possible (TREE_TYPE (TREE_OPERAND (stmt, 0)),
	      TREE_OPERAND (func, 3)))
	      {
		tree t = make_binary_op (CONVERT_EXPR, TREE_OPERAND (stmt, 0), 
		    TREE_OPERAND (func, 3));
		TREE_OPERAND_SET (stmt, 0, t);
	      }
	    else
	      {
		error_loc (TREE_LOCATION (TREE_OPERAND (stmt, 0)), 
			  "wrong return value type `%s'",
		  TREE_CODE_NAME (TREE_CODE (TREE_TYPE (TREE_OPERAND (stmt, 0)))));
		return 1;
	      }
	  }
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
  tree ext_vars = NULL;
  tree vars = make_tree_list ();
  unsigned type_counter = 0, var_counter = 0;
  int ret = 0;
  assert (TREE_CODE (func) == FUNCTION, "function tree expected");
  
  /* get argument names.  */
  ext_vars = TREE_OPERAND (func, 1);
  /* get argument types.  */
  if (TREE_OPERAND (func, 2) != NULL)
    type = TREE_LIST (TREE_OPERAND (func, 2));
  else
    type = NULL;
  
  /* Check and assign types to arguments.  */
  if (ext_vars != NULL)
    DL_FOREACH (TREE_LIST (ext_vars), el)
      {
	/* check for identifier duplicates in argument list.  */
	tree dup = is_var_in_list (el->entry, ext_vars);
	if (dup != NULL && dup != el->entry)
	  {
	    error_loc (TREE_LOCATION (func), "argument `%s' occures more "
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
	else
	  {
	    int ret_val = 0;
	    tree ext_vars = make_tree_list ();
	    tree vars = make_tree_list ();
	    ret_val = typecheck_type (type->entry, ext_vars, vars);
	     
	    tree_list_append (delete_list, ext_vars);
	    tree_list_append (delete_list, vars);

	    if (ret_val)
	      return ret_val;
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

  ret =typecheck_stmt_list (TREE_OPERAND (func, 4), ext_vars, vars, func);
 
  /* Free list with local variables.  */
  tree_list_append (delete_list, vars);
  return ret;
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
	    ret += 1;
	  }
      }
      break;
    case FUNCTION_CALL:
      {
	tree t;
	struct tree_list_element *func_el, *expr_el;
	int func_counter = 0, expr_counter = 0;

	/* check if function is declared.  */
	if ((t = function_exists (TREE_STRING_CST 
	      (TREE_ID_NAME ((TREE_OPERAND (expr, 0)))))) == NULL)
	  {
	    error_loc (TREE_LOCATION (expr), 
		    "function `%s' is not defined",
		    TREE_STRING_CST (TREE_ID_NAME ((TREE_OPERAND (expr, 0)))));
	    return 1;
	  }

	/* argument list can be empty.  */
	if (TREE_OPERAND (t, 2) != NULL)
	  func_el = TREE_LIST (TREE_OPERAND (t, 2));
	else
	  func_el = NULL;
	
	/* check argument number and corresponding types.  */
	if (TREE_OPERAND (expr, 1) != NULL)
	  {
	    DL_FOREACH (TREE_LIST (TREE_OPERAND (expr, 1)), expr_el)
	      {
		int ret = 0;
      
		if ((ret = typecheck_expression (expr_el->entry, 
			      ext_vars, vars)))
		  return 1;

		expr_counter++;
		if (func_el == NULL)
		  {
		    while (expr_el->next != NULL)
		      {
			expr_counter++;
			expr_el = expr_el->next;
		      }
		      
		    error_loc (TREE_LOCATION (expr),
		      "invalid number of arguments in function `%s' call: "
		      "%u expected, %u found",
		      TREE_STRING_CST (TREE_ID_NAME (TREE_OPERAND (t, 0))),
		      func_counter, expr_counter);
		    return 1;
		  }
		if (!tree_compare (func_el->entry, 
				 TREE_TYPE (expr_el->entry)))
		    {
		      /* try to convert types.  */
		      if (conversion_possible 
			    (TREE_TYPE (expr_el->entry), func_el->entry))
			{
			  tree t = make_binary_op (CONVERT_EXPR,
				    expr_el->entry, func_el->entry);
			  TREE_OPERAND_SET (expr, 1, t);
			}
		      else
			{
			  error_loc (TREE_LOCATION (expr_el->entry), 
				"argument %u type mismatch: "
				"`%s' expected, `%s' found",
				expr_counter,
				TREE_CODE_NAME (TREE_CODE (func_el->entry)),
				TREE_CODE_NAME (TREE_CODE (TREE_TYPE 
				 (expr_el->entry))));
			  return 1;
		      }
		    }
		  func_counter++;
		  func_el = func_el->next;
	      }
	  }
	if (func_el != NULL)
          {
	    while (func_el != NULL)
	      {
	        func_counter++;
	        func_el = func_el->next;
	      }
	    error_loc (TREE_LOCATION (expr),
		"invalid number of arguments in function '%s' call: "
		"%u expected, %u found",
		TREE_STRING_CST (TREE_ID_NAME (TREE_OPERAND (t, 0))),
		func_counter, expr_counter);
	    return 1;
	  }
	
	TREE_TYPE (expr) = TREE_OPERAND (t, 3);
      }
      break;
    case EQ_EXPR:
    case GT_EXPR:
    case LT_EXPR:
    case GE_EXPR:
    case LE_EXPR:
    case NE_EXPR:
      {
	tree lhs = TREE_OPERAND (expr, 0);
	tree rhs = TREE_OPERAND (expr, 1);

	ret += typecheck_expression (lhs, ext_vars, vars);
	ret += typecheck_expression (rhs, ext_vars, vars);

	if (ret != 0 || TREE_TYPE (lhs) == NULL || TREE_TYPE (rhs) == NULL)
	  return 1;

	if (!tree_compare (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	  {
	    /* try to convert types.  */
	    if (conversion_possible 
		    (TREE_TYPE (rhs), TREE_TYPE (lhs)))
	      {
		tree t = make_binary_op (CONVERT_EXPR,
		    	    rhs, TREE_TYPE (lhs));
		TREE_OPERAND_SET (expr, 1, t);
	      }
	    else
	      {
		/* FIXME Type printing is proper only for primitive types.  */
		error_loc (TREE_LOCATION (lhs), "type mismatch. the left operand "
					    "is `%s', the right operand is `%s'",
		      TREE_CODE_NAME (TREE_CODE (TREE_TYPE (lhs))),
		      TREE_CODE_NAME (TREE_CODE (TREE_TYPE (rhs))));
		return 1;
	     }
	  }
	
	TREE_TYPE (expr) = b_type_node;
      }
      break;
    case LAND_EXPR:
    case LOR_EXPR:
      {
	tree lhs = TREE_OPERAND (expr, 0);
	tree rhs = TREE_OPERAND (expr, 1);

	ret += typecheck_expression (lhs, ext_vars, vars);
	ret += typecheck_expression (rhs, ext_vars, vars);

	if (ret != 0 || TREE_TYPE (lhs) == NULL || TREE_TYPE (rhs) == NULL)
	  return 1;

	if (TREE_TYPE (lhs) != b_type_node
	 || TREE_TYPE (rhs) != b_type_node)
	  {
	    /* FIXME Type printing is proper only for primitive types.  */
	    error_loc (TREE_LOCATION (lhs), "expected boolean expressions. "
					    "The left operand is `%s', "
					    "the right operand is `%s'.",
		      TREE_CODE_NAME (TREE_CODE (TREE_TYPE (lhs))),
		      TREE_CODE_NAME (TREE_CODE (TREE_TYPE (rhs))));
	    return 1;
	  }
	else
	  TREE_TYPE (expr) = b_type_node;
      }
      break;
    case PLUS_EXPR:
    case MINUS_EXPR:
    case DIV_EXPR:
    case MULT_EXPR:
    case MOD_EXPR:
    case SLEFT_EXPR:
    case SRIGHT_EXPR:
    case BAND_EXPR:
    case BOR_EXPR:
    case XOR_EXPR:
      {
	tree lhs = TREE_OPERAND (expr, 0);
	tree rhs = TREE_OPERAND (expr, 1);

	ret += typecheck_expression (lhs, ext_vars, vars);
	ret += typecheck_expression (rhs, ext_vars, vars);

	if (ret != 0 || TREE_TYPE (lhs) == NULL || TREE_TYPE (rhs) == NULL)
	  return 1;

	if (!tree_compare (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	  {
	    /* try to convert types.  */
	    if (conversion_possible 
		    (TREE_TYPE (rhs), TREE_TYPE (lhs)))
	      {
		tree t = make_binary_op (CONVERT_EXPR,
		    	    rhs, TREE_TYPE (lhs));
		TREE_OPERAND_SET (expr, 1, t);
	      }
	    else 
	      {
		error_loc (TREE_LOCATION (lhs), "type mismatch. the left operand "
					    "is `%s', the right operand is `%s'",
		      TREE_CODE_NAME (TREE_CODE (TREE_TYPE (lhs))),
		      TREE_CODE_NAME (TREE_CODE (TREE_TYPE (rhs))));
		return 1;
	      }
	  }
	TREE_TYPE (expr) = TREE_TYPE (lhs);
      }
      break;
    /* unary operations.  */
    case UMINUS_EXPR:
    case NOT_EXPR:
      {
	tree op = TREE_OPERAND (expr, 0);
	ret += typecheck_expression (op, ext_vars, vars);

	if (ret != 0 || TREE_TYPE (op) == NULL)
	  return 1;

	TREE_TYPE (expr) = TREE_TYPE (op);
      }
      break;
    case CIRCUMFLEX:
      {
	tree lhs = TREE_OPERAND (expr, 0);
	tree rhs = TREE_OPERAND (expr, 1);

	ret += typecheck_expression (lhs, ext_vars, vars);
	ret += typecheck_expression (rhs, ext_vars, vars);

	if (ret != 0 || TREE_TYPE (lhs) == NULL || TREE_TYPE (rhs) == NULL)
	  return 1;

	/* This is a recurrent variable.  */
	if (TREE_CIRCUMFLEX_INDEX_STATUS (expr))
	  {
	    if (TREE_TYPE (rhs) != z_type_node)
	      {
		/* FIXME Type printing is proper only for primitive types.  */
		error_loc (TREE_LOCATION (lhs), "index of a recurrent variable "
						"must be '%s`, not `%s'",
				TREE_CODE_NAME (TREE_CODE (z_type_node)),
				TREE_CODE_NAME (TREE_CODE (TREE_TYPE (rhs))));
		return 1;
	      }
	    TREE_TYPE (expr) = TREE_TYPE (lhs);
	  }
	else
	  {
	    /* This is the only case when we set the result of power operation
	       to the integer type.  */
	    if  (TREE_TYPE (lhs) == z_type_node
		&& TREE_TYPE (rhs) == z_type_node
		&& TREE_CONSTANT (rhs)
		&& TREE_INTEGER_CST (rhs) >= 0)
	      TREE_TYPE (expr) = z_type_node;
	    else
	      TREE_TYPE (expr) = r_type_node;
	}
      }
      break;
    case LOWER:
      {
	struct tree_list_element *el = NULL, *index_el = NULL;
	tree lhs = TREE_OPERAND (expr, 0);
	tree rhs = TREE_OPERAND (expr, 1);
	tree dim_t = NULL;
	int dim = 0;
	int i; 
	tree shape = NULL;

	ret += typecheck_expression (lhs, ext_vars, vars);
  
	/* Indexes usage is valid only for vector types.  */
	if  (TYPE_DIM (TREE_TYPE (lhs)) == NULL)
	  {
	    error_loc (TREE_LOCATION (lhs), "index operations are valid "
					   "for vector types only");
	    return 1;
	  }
	else
	  dim = TREE_INTEGER_CST (TYPE_DIM (TREE_TYPE (lhs)));

	/* The return type of index operation is constructed using the
	   information about base type and index list.
	   Code  -- remains the same.
	   Dim   -- defined as <dim of base> - <length of index list>
		    (in case of 0, assign NULL, not an empty list).
	   Shape -- contains the tail of base type shape, where the number of
		    elements are defined by "dim".
		    (i.e. if the base type has {dim = 3; shape = {4, 3, 1}} and
		    we perform indexing as _{3, 1}, then the resulting type
		    will have {dim = 1; shape = {1}}).  */
	if (TYPE_SHAPE (TREE_TYPE (lhs)) != NULL)
	  el = TREE_LIST (TYPE_SHAPE (TREE_TYPE (lhs)));
	DL_FOREACH (TREE_LIST (rhs), index_el)
	  {
	    tree t = index_el->entry;
	  
	    if (--dim <= -1)
	      {
		error_loc (TREE_LOCATION (t), "the number of indexes can't"
					      " exceed type dimension, "
					      "which is %d",
				TREE_INTEGER_CST (TYPE_DIM (TREE_TYPE (lhs))));
		return ret + 1;	
	      }

	    ret += typecheck_expression (t, ext_vars, vars);
	    /* Index is not the integer.  */
	    if (TREE_TYPE (t) != z_type_node)
	      {
		/* FIXME Type printing is proper only for primitive types.  */
		error_loc (TREE_LOCATION (t), "the index must be '%s',"
					      " not`%s'",
			      TREE_CODE_NAME (TREE_CODE (z_type_node)),
			      TREE_CODE_NAME (TREE_CODE (TREE_TYPE (t))));
		return ret + 1;
	      }
	    /* check boundaries if both index and shape element are
	       constants.  */
	    if (el != NULL)
	      {
		if (TREE_CONSTANT (t) && TREE_CONSTANT (el->entry) &&
		    TREE_INTEGER_CST (t)
		    - TREE_INTEGER_CST (el->entry) >= 0)
	          {
		    error_loc (TREE_LOCATION (index_el->entry), 
		        "array index is beyond its boundaries");
		    ret += 1;
		  }
		el = el->next;
	      }
	  }

	if (dim > 0 && TYPE_SHAPE (TREE_TYPE (lhs)) != NULL)
	  {
	    i = 0;
	
	    for (el = el->next; el != NULL ; el = el->next)
	      {
		if (shape == NULL)
		  shape = make_tree_list();
		tree_list_append (shape, el->entry);
	      }
	  }

	if (dim)
	  dim_t = make_integer_cst (dim);

	TREE_TYPE (expr) = types_assign_type (TREE_CODE (TREE_TYPE (lhs)), 
				    TYPE_SIZE (TREE_TYPE (lhs)),
				    dim ? make_integer_cst (dim) : NULL,
				    shape);
 
	if (dim_t != TYPE_DIM (TREE_TYPE (expr)))
	  free_tree (dim_t);
	if (shape != TYPE_SHAPE (TREE_TYPE (expr)))
	  free_tree (shape);

      }
      break;
    case MATRIX_EXPR:
      {
	tree el_list = TREE_OPERAND (expr, 0);
	struct tree_list_element *l, *el;
	unsigned shape_x, shape_y = 0;
	enum tree_code code = 0;
	size_t size = 0;
	tree shape_t, dim_t;
	
	DL_FOREACH (TREE_LIST (el_list), l)
	  {
	    shape_x = 0;
	    DL_FOREACH (TREE_LIST (l->entry), el)
	      {
		ret += typecheck_expression (el->entry, ext_vars, vars);

		if  (TYPE_DIM (TREE_TYPE (el->entry)) != NULL
		||  TYPE_SHAPE (TREE_TYPE (el->entry)) != NULL)
		  {
		    error_loc (TREE_LOCATION (el->entry),
			"vector types are not supported in arrays at the moment");
		    ret += 1;
		  }

		if (!code)
		  code = TREE_CODE (TREE_TYPE (el->entry));
		else if (TREE_CODE (TREE_TYPE (el->entry)) != code)
		  {
		    /* FIXME type conversion must be performed here.  */
		    error_loc (TREE_LOCATION (el->entry), 
				"type mismatch: `%s' expected, `%s' found",
				TREE_CODE_NAME (code),
				TREE_CODE_NAME (TREE_CODE (TREE_TYPE
		      		(el->entry))));
		    ret += 1;
		  }
		if (!size)
		  size = TYPE_SIZE (TREE_TYPE (el->entry));
		shape_x++;
	      }
	    shape_y++;
	  }

	if (shape_x == 1)	
	  dim_t = make_integer_cst (1);
	else
	  dim_t = make_integer_cst (2);

	shape_t = make_tree_list ();
	if (shape_x != 1)
	  tree_list_append (shape_t, make_integer_cst (shape_y));
	tree_list_append (shape_t, make_integer_cst (shape_x));
	
	TREE_TYPE (expr) = types_assign_type (code, size, dim_t, shape_t);
	
	if (dim_t != TYPE_DIM (TREE_TYPE (expr)))
	  free_tree (dim_t);
	if (shape_t != TYPE_SHAPE (TREE_TYPE (expr)))
	  free_tree (shape_t);
      }
      break;
    default:
      /* FIXME Type printing is proper only for primitive types.  */
      error ("cannot typecheck expression of type `%s'",
	     TREE_CODE_NAME (TREE_CODE (expr)));
      unreachable (0);
    }

  return ret;
}
