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
#include "typecheck.h"
#include "print.h"
#include "types.h"
#include "recurrence.h"

static int associate_variables (tree, tree, tree);

extern tree iter_var_list;

static struct
typecheck_options
{
  bool iter_index;
} typecheck_options;

static void 
init_typecheck_options ()
{
  /* indicate that recurrent expressions is being typechecked now.  */
  typecheck_options.iter_index = false;
}

int
typecheck ()
{
  struct tree_list_element *tl;
  int function_check = 0;
  init_typecheck_options ();

  DL_FOREACH (TREE_LIST (function_list), tl)
    function_check += typecheck_function (tl->entry);

  /* sort recurrent expressions by initial values.  */
  DL_FOREACH (TREE_LIST (iter_var_list), tl)
    DL_SORT (TREE_LIST (TREE_ITER_LIST (TREE_ID_ITER (tl->entry))), 
	     recurrence_sort);

  /* validate recurrent expressions.  */
  DL_FOREACH (TREE_LIST (iter_var_list), tl)
    {
      recurrence_validate (tl->entry);
  //    recurrence_check_window (tl->entry);
  //    recurrence_check_precedence (tl->entry);
  //    recurrence_check_initial (tl->entry);
    }

  if (function_check || error_count > 0)
    printf ("note: finished typechecking, %i error(s) found.\n",
	    error_count);
  else
    printf ("note: finished typechecking  [ok].\n");

  return function_check || error_count;
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

/* Check either type conversion is possible.
   NOTE types are not validated here.  */
static inline bool
conversion_possible (tree from, tree to)
{
 
  if ((from == z_type_node || from == n_type_node) && to == r_type_node)
    return true;

	
  if (TYPE_DIM (from) != NULL
      && TYPE_DIM (to) != NULL)
    {
      if (tree_compare (TYPE_DIM (from), TYPE_DIM (to)))
	{
	  if ((tree_compare (TYPE_SHAPE (from), TYPE_SHAPE (to))
	       || TYPE_SHAPE (to) == NULL)
	      && ((TREE_CODE (from) == Z_TYPE || TREE_CODE (from) == N_TYPE)
		  && (TREE_CODE (to) == R_TYPE || TREE_CODE (to) == Z_TYPE
		      || TREE_CODE (to) == N_TYPE)))
	    return true;

	  /* if shape is marked as an unknown, we propose that conversion is
	     possible, however, it has to be checked later.  */
	  if (TYPE_SHAPE (from) == unknown_mark_node)
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
      if (TREE_TYPE (TYPE_DIM (type)) != z_type_node
	  && (TREE_TYPE (TYPE_DIM (type)) != n_type_node
	      || TYPE_DIM (type) != NULL
	      || TYPE_SHAPE (type) != NULL))
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
	      || TYPE_DIM (TREE_TYPE (t)) != NULL
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
typecheck_recurrent (tree expr)
{
  if (TREE_CODE (expr) == MINUS_EXPR)
    {
      if (TREE_OPERAND (expr, 0) != iter_var_node
       || TREE_CODE (TREE_OPERAND (expr, 1)) != INTEGER_CST)
	goto error;
      /* `\iter' node is global. Therfore the type is assigned globally.  */
      TREE_TYPE (TREE_OPERAND (expr, 1)) = z_type_node;
    }
  else if (TREE_CODE (expr) != INTEGER_CST
	&& expr != iter_var_node)
    goto error;

  TREE_TYPE (expr) = z_type_node;
  return 0;

error:
  error_loc (TREE_LOCATION (expr), "the index of recurrent expression must"
    " be either a constant or a linear expression with "
    "`%s' and a constant", token_kind_name[tv_iter]);
  return 1;
}

#if 0
/* Typecheck assign statement if the left operand is an identifier.  */
int
typecheck_assign_identifier (tree stmt, tree ext_vars, tree vars)
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


  /* Check if the variable was defined locally. */
  if ((var = is_var_in_list (lhs, vars)) != NULL
      || (var = is_var_in_list (lhs, ext_vars)) != NULL)
    {
      /* Replace the variable with the variable from the list. */
      TREE_OPERAND_SET (stmt, 0, var);
      free_tree (lhs);
      lhs = TREE_OPERAND (stmt, 0);
    }
  else
    tree_list_append (ext_vars, lhs);

  /* FIXME Check that these types are not NULL. */
  if (TREE_TYPE (lhs) == NULL)
    TREE_TYPE (lhs) = TREE_TYPE (rhs);
  else if (!tree_compare (TREE_TYPE(lhs), TREE_TYPE(rhs)))
    {
      /* try to convert types. */
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
#endif

int
typecheck_stmt_assign_right (struct tree_list_element *el, 
				    tree ext_vars, tree vars)
{
  int ret = 0;
  tree rhs = el->entry;
  typecheck_options.iter_index = true;
  ret += typecheck_expression (rhs, ext_vars, vars);
  if (ret)
    return ret;
  if (TREE_CODE (rhs) == FUNCTION_CALL && TREE_CODE (TREE_TYPE (rhs)) == LIST)
    {
      tree list = make_tree_list ();
      tree_list_append(list, rhs);
      rhs = list;
    }
  typecheck_options.iter_index = false;
  return ret;
}

int
typecheck_stmt_assign_left (struct tree_list_element *el, 
				   tree ext_vars, tree vars, tree *id)
{
  int ret = 0;
  tree lhs = el->entry;
  int tmp_ret;

  if (TREE_CODE (lhs) == IDENTIFIER)
    {
      tree var;

      /* Check if the variable was defined locally. */
      if ((var = is_var_in_list (lhs, vars)) != NULL
	  || (var = is_var_in_list (lhs, ext_vars)) != NULL)
	{
	  /* Replace the variable with the variable from the list. */
	  free_tree (lhs);
	  el->entry = var;
	  lhs = var;
	}
      else
	tree_list_append (ext_vars, lhs);
      TREE_ID_DEFINED (lhs) = true;
    }
  else if (TREE_CODE (lhs) == CIRCUMFLEX)
    {
      tree var, index = TREE_OPERAND (lhs, 1);

      if (index != iter_var_node
	  && TREE_CODE (index) != INTEGER_CST)
	{
	  error_loc (TREE_LOCATION (index),
		     "only `%s' or integer constant is allowed here",
		     token_kind_name[tv_iter]);
	  ret += 1;
	  return ret;
	}

      if (TREE_CODE (index) == INTEGER_CST)
	TREE_TYPE (index) = z_type_node;

      typecheck_options.iter_index = true;

      tmp_ret = typecheck_expression (TREE_OPERAND (lhs, 0), ext_vars,
				      vars);
      if (tmp_ret)
	{
	  ret += 1;
	  return ret;
	}

      assert (TREE_CODE (TREE_OPERAND (lhs, 0)) == IDENTIFIER,
	      "identifier expected");
      *id = TREE_OPERAND (lhs, 0);

      if ((var = is_var_in_list (*id, vars)) != NULL
       || (var = is_var_in_list (*id, ext_vars)) != NULL)
	{
	  free_tree (*id);
	  TREE_OPERAND_SET (lhs, 0, var);
	  *id = var;
	}
      TREE_ID_DEFINED (*id) = true;

      TREE_TYPE (lhs) = TREE_TYPE (TREE_OPERAND (lhs, 0));
    }
  return ret;
}

int
typecheck_assign_index (tree lhs, tree rhs, tree id) 
{
  int ret = 0;
  /* Assign index information to identifier.  */
  if (TREE_CODE (lhs) == CIRCUMFLEX)
    {
      if (TREE_ID_ITER (id) == NULL)
	{
	  TREE_ID_ITER (id) = make_tree (ITER_EXPR);
	  TREE_ITER_LIST (TREE_ID_ITER (id)) = make_tree_list ();
	  tree_list_append (iter_var_list, id);
	}
      else
	{
	  struct tree_list_element *el;
	  DL_FOREACH (TREE_LIST (TREE_ITER_LIST (TREE_ID_ITER (id))), el)
	    {
	      tree index = TREE_OPERAND (el->entry, 0);
	      if ((TREE_OPERAND (lhs, 1) == iter_var_node
		  && index == iter_var_node)
		  || (TREE_CODE (TREE_OPERAND (lhs, 1)) == INTEGER_CST
		  && TREE_CODE (index) == INTEGER_CST
		  && TREE_INTEGER_CST (TREE_OPERAND (lhs, 1))
		      == TREE_INTEGER_CST (index)))
		{
		  char* s = tree_to_str (index);
		  error_loc (TREE_LOCATION (TREE_OPERAND (lhs, 1)),
			     "index `%s' is defined already for "
			     "variable `%s'",
			     s,
			     TREE_STRING_CST (TREE_ID_NAME (id)));
		  ret += 1;
		  free (s);
		}
	    }
	}
      tree_list_append (TREE_ITER_LIST (TREE_ID_ITER (id)),
	  make_binary_op (ITER_PAIR, TREE_OPERAND (lhs, 1), rhs));
    }
  return ret;
}

int
typecheck_stmt (tree stmt, tree ext_vars, tree vars, tree func)
{
  int ret = 0;

  switch (TREE_CODE (stmt))
    {
    case ASSIGN_STMT:
      {
	tree id = NULL;

	struct tree_list_element *lel = TREE_LIST (TREE_OPERAND (stmt, 0));
	struct tree_list_element *rel = TREE_LIST (TREE_OPERAND (stmt, 1));

	while (lel != NULL && rel != NULL)
	  {
	    tree lhs;
	    tree rhs;
	    ret += typecheck_stmt_assign_left (lel, ext_vars, vars, &id);
	    ret += typecheck_stmt_assign_right (rel, ext_vars, vars);
	    
	    if (ret)
	      return ret;

	    lhs = lel->entry;
	    rhs = rel->entry;

	    ret += typecheck_assign_index (lhs, rhs, id);

	    if (ret)
	      return ret;
	    if (TREE_CODE  (TREE_TYPE (rhs)) != LIST)
	      {
		if (TREE_TYPE (lhs) == NULL)
		  TREE_TYPE (lhs) = TREE_TYPE (rhs);
		else if (!tree_compare (TREE_TYPE(lhs), TREE_TYPE(rhs)))
		  {
		    /* try to convert types. */
		    if (conversion_possible (TREE_TYPE (rhs), TREE_TYPE (lhs)))
		      {
			tree t = make_binary_op (CONVERT_EXPR, 
						 rhs, TREE_TYPE (lhs));
			rhs = t;
			//TREE_OPERAND_SET (stmt, 1, t);
		      }
		    else
		      {
			error_loc (TREE_LOCATION (rhs),
				   "Assignment left hand side type does not "
				   "match right hand side type");
			return ret + 1;
		      }
		  }
	      }
	    else
	      {
		struct tree_list_element *el;
		DL_FOREACH (TREE_LIST (TREE_TYPE (rhs)), el)
		  {
		    lhs = lel->entry;
		    if (TREE_TYPE (lhs) == NULL)
		      TREE_TYPE (lhs) = el->entry;
		    else if (!tree_compare (TREE_TYPE (lhs), el->entry))
		      {
			/* try to convert types. */
			if (conversion_possible (el->entry,
						 TREE_TYPE (lhs)))
			  {
			    tree t = make_binary_op (CONVERT_EXPR, el->entry,
			    TREE_TYPE (lhs));
			    el->entry = t;
			  }
			else
			  {
			    error_loc (TREE_LOCATION (rhs),
				   "Assignment left hand side type does not "
				   "match right hand side type");
			    return ret + 1;
			  }
		      }
		      if (el->next != NULL)
			{
			  lel = lel->next;
			  ret += typecheck_assign_index (lel->entry, rhs, id);
			}
		  }
	      }

	    lel = lel->next;
	    rel = rel->next;
	  }
	if ((lel == NULL) && (rel != NULL))
	  {
	    error_loc (TREE_LOCATION (rel->entry),
	      "the number of expressions in the right part is more than there "
	      "are identifiers in the left part of the statement");
	    return 1;
	  }
	else if ((lel != NULL) && (rel == NULL))
	  {
	    error_loc (TREE_LOCATION (lel->entry),
	      "the number of expressions in the right part is less than there "
	      "are identifiers in the left part of the statement");
	    return 1;
	  }

	return ret;
      }
      break;

    case DECLARE_STMT:
      {
	struct tree_list_element *lel = TREE_LIST (TREE_OPERAND (stmt, 0));
	struct tree_list_element *rel = TREE_LIST (TREE_OPERAND (stmt, 1));

	while (lel != NULL && rel != NULL)
	  {
	    tree var;
	    tree lhs = lel->entry;
	    tree rhs = rel->entry;

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
	  
	    lel = lel->next;
	    rel = rel->next;
	  }
	if ((lel == NULL) && (rel != NULL))
	  {
	    error_loc (TREE_LOCATION (rel->entry),
	      "the number of types in the right part is more than there are "
	      "identifiers in the left part of the statement");
	    return 1;
	  }
	else if ((lel != NULL) && (rel == NULL))
	  {
	    error_loc (TREE_LOCATION (lel->entry),
	      "the number of types in the right part is less than there are "
	      "identifiers in the left part of the statement");
	    return 1;
	  }
      }
      break;

    case INDEX_LOOP_EXPR:
      {
	struct tree_list_element *el, *tmp;
	tree var = TREE_OPERAND (stmt, 0);
	tree gen = TREE_OPERAND (stmt, 1);
	tree cases = TREE_OPERAND (stmt, 2);
	tree new_scope = make_tree_list ();
	tree lower = TREE_OPERAND (stmt, 0);

	tree_list_combine (ext_vars, vars);

	if (TREE_CODE (var) == CIRCUMFLEX)
	  {
	    tree index = TREE_OPERAND (var, 1);
	    lower = TREE_OPERAND (var, 0);

	    assert (TREE_CODE (index) == IDENTIFIER,
		    "recurrent index must be identifier");
	    assert (TREE_CODE (lower) == LOWER,
		    "lower indexes must be mentioned");

	    if (!TREE_CIRCUMFLEX_INDEX_STATUS (var))
	      {
		error_loc (TREE_LOCATION (var), "only recurrent relation "
					    "is allowed here");
		ret += 1;
	      }

	    TREE_TYPE (index) = z_type_node;
	    tree_list_append (new_scope, index);
	  }
	
	ret += typecheck_lower (lower, ext_vars, new_scope, true);
	ret += associate_variables (lower, ext_vars, new_scope);
	if(ret)
	  goto finalize_withloop;

	/* check generator.  */
	ret += typecheck_generator (gen, ext_vars, new_scope);
	if (ret)
	  goto finalize_withloop;

	DL_FOREACH (TREE_LIST (cases), el)
	  {
	    tree exp = TREE_OPERAND (el->entry, 0);
	    ret += typecheck_expression (exp, ext_vars, new_scope);
	    if (TREE_CODE (TREE_OPERAND (el->entry, 1)) == GENERATOR)
	      ret += typecheck_generator (TREE_OPERAND (el->entry, 1),
					  ext_vars, new_scope);

	    if (!tree_compare (TREE_TYPE (var), TREE_TYPE (exp)))
	      {
		char* var_type = tree_to_str (TREE_TYPE (var));
		char* exp_type = tree_to_str (TREE_TYPE (exp));
		error_loc (TREE_LOCATION (exp), "type mismatch. "
			   "`%s' expected, `%s' found.",
			   var_type, exp_type);
		free (var_type);
		free (exp_type);
		ret += 1;
	      }

	  }

finalize_withloop:
	DL_FOREACH_SAFE (TREE_LIST (new_scope), el, tmp)
	  {
	    DL_DELETE (TREE_LIST (new_scope), el);
	    free (el);
	  }

	free_tree (new_scope);

	/* split combined lists back.  */
	tree_list_split (ext_vars, vars);
	return ret;
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
	struct tree_list_element *ret_el = TREE_LIST (TREE_OPERAND (stmt, 0));
	struct tree_list_element *type_el = TREE_LIST (TREE_OPERAND(func, 3));
	while (ret_el != NULL && type_el != NULL)
	  {
	    ret += typecheck_expression (ret_el->entry, ext_vars, vars);
	    if (ret)
	      return ret;
	    if (!tree_compare (TREE_TYPE (ret_el->entry),
		  type_el->entry))
	      {
		if (conversion_possible (TREE_TYPE (ret_el->entry),
					 type_el->entry))
		  {
		    tree t = make_binary_op (CONVERT_EXPR, ret_el->entry,
					     type_el->entry);
		    ret_el->entry = t;
		    //TREE_OPERAND_SET (stmt, 0, t);
		  }
		else
		  {
		    char* type = tree_to_str (TREE_TYPE (ret_el->entry));
		    error_loc (TREE_LOCATION (ret_el->entry),
			      "wrong return value type `%s'", type);
		    free (type);
		    return 1;
		  }
	      }
	    ret_el = ret_el->next;
	    type_el = type_el->next;
	  }
	if ((ret_el == NULL) ^ (type_el == NULL))
	  {
	    error_loc (TREE_LOCATION (TREE_LIST(
		  TREE_OPERAND (stmt, 0))->entry),
	      "the number of return values doesn't match with function ",
	      "definition");
	    return 1;
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

  ext_vars = tree_copy (TREE_OPERAND (func, 1));
  ret =typecheck_stmt_list (TREE_OPERAND (func, 4), ext_vars, vars, func);

  /* Free list with local variables.  */
  tree_list_append (delete_list, vars);
  tree_list_append (delete_list, ext_vars);
  return ret;
}

int
typecheck_lower (tree expr, tree ext_vars, tree vars, bool generator)
{
  int ret = 0;
  struct tree_list_element *el = NULL, *index_el = NULL;
  
  tree lhs = TREE_OPERAND (expr, 0);
  tree rhs = TREE_OPERAND (expr, 1);
  tree dim_t = NULL;
  int dim = 0;
  int i;
  tree shape = NULL;

  /* if 'generator' is not set, it means that this function was called from
     generator expression. Then 'lhs' must be an identifier.  */
  if (generator)
    {
      /* FIXME: Should not be in assert.  */
      assert (TREE_CODE (lhs) == IDENTIFIER, "only identifier is valid here");
    }

  ret += typecheck_expression (lhs, ext_vars, vars);
  if (ret)
    return ret;

  /* it is possible to use indexes only with vector types.  */
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
	  error_loc (TREE_LOCATION (t), "the number of indexes can't "
		     "exceed type dimension, which is %d",
		     TREE_INTEGER_CST (TYPE_DIM (TREE_TYPE (lhs))));
	  return ret + 1;
	}

      if (generator)
	{
	  assert (TREE_CODE (t) == IDENTIFIER, "only identifier is valid here");

	  if (!is_var_in_list (t, vars))
	    {
	      TREE_TYPE (t) = z_type_node;
	      tree_list_append (vars, t);
	    }
	  else
	    {
	      error_loc (TREE_LOCATION (t), "variable `%s' is defined more "
			 "than once in this scope",
			 TREE_STRING_CST (TREE_ID_NAME (t)));
	      ret += 1;
	    }
	}
      else
	{
	  ret += typecheck_expression (t, ext_vars, vars);
	  if (ret)
	    return ret;
	}

      /* Index is not the integer.  */
      if (TREE_TYPE (t) != z_type_node)
	{
	  char* z_type = tree_to_str (z_type_node);
	  char* t_type = tree_to_str (TREE_TYPE (t));

	  error_loc (TREE_LOCATION (t), "the index must be `%s', not`%s'",
		     z_type, t_type);
	  free (z_type);
	  free (t_type);
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
		  return ret;
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

  /* We consider indexing operation to be non-constant,
     as we can't really check it at this point.  */
  TREE_CONSTANT (expr) = false;
  if (dim_t != TYPE_DIM (TREE_TYPE (expr)))
    free_tree (dim_t);
  if (shape != TYPE_SHAPE (TREE_TYPE (expr)))
    free_tree (shape);

  return ret;
}

static int 
associate_variables (tree t, tree ext_vars, tree vars)
{
  tree var;
  int i = 0;
  for(; i < TREE_CODE_OPERANDS (TREE_CODE (t)); i++)
    {
      tree id = TREE_OPERAND (t, i);
      if (TREE_CODE (id) == IDENTIFIER)
	{
	  if ((var = is_var_in_list (id, vars)) != NULL
	      || (var = is_var_in_list (id, ext_vars)) != NULL)
	    {
	      if (var != id)
		{
		  tree_list_append (delete_list, id);
		  TREE_OPERAND_SET (t, i, var);
		}
	    }
	}
      else
	associate_variables (id, ext_vars, vars);
    }
  return 0;
}

int
typecheck_generator (tree expr, tree ext_vars, tree vars)
{
  int ret = 0;
  struct tree_list_element *el;
  tree var;

  assert (TREE_CODE (expr) == FORALL || TREE_CODE (expr) == GENERATOR,
	  "generator tree must be either 'generator' or 'type'");
  DL_FOREACH (TREE_LIST (TREE_OPERAND (expr, 0)), el)
    {
      assert (TREE_CODE (el->entry) == IDENTIFIER,
	      "only identifiers must be mentioned in generator");

      /* variables can't be duplicated in the list.  */
      if ((var = is_var_in_list (el->entry, TREE_OPERAND (expr, 0))) != NULL
        && var != el->entry)
	{
	  error_loc (TREE_LOCATION (el->entry),
		     "variable `%s' occurs more than once in the list",
		     TREE_STRING_CST (TREE_ID_NAME (el->entry)));
	  ret += 1; 

	}
      if ((var = is_var_in_list (el->entry, vars)) == NULL)
	{
	  error_loc (TREE_LOCATION (el->entry),
		     "variable `%s' is undefined",
		     TREE_STRING_CST (TREE_ID_NAME (el->entry)));
	  ret += 1;
	}
      else
	{
	  free_tree (el->entry);
	  el->entry = var;
	  if (TREE_TYPE (el->entry) != z_type_node
	      && TREE_TYPE (el->entry) != n_type_node)
	    {
	      error_loc (TREE_LOCATION (el->entry),
			 "variable `%s' invalid type",
			 TREE_STRING_CST (TREE_ID_NAME (el->entry)));
	      ret += 1;
	    }
	}
    }

  if (TREE_CODE (expr) == GENERATOR)
    {
      tree exp = TREE_OPERAND (expr, 1);
      ret += typecheck_expression (exp, ext_vars, vars);
      associate_variables (exp, ext_vars, vars);
    }
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
      TREE_TYPE (expr) =
	types_assign_type (STRING_TYPE,
			   strlen(TREE_STRING_CST (expr)) + 1, NULL, NULL);
      TREE_CONSTANT (expr) = true;
      break;
    case IDENTIFIER:
      {
	tree var;

	if (expr == iter_var_node)
	  {
	    if (!typecheck_options.iter_index)
	      {
		error_loc (TREE_LOCATION (expr), "`%s' is allowed only in the "
		  "definition of recurrent process",
		  token_kind_name[tv_iter]);
		return 1;
	      }
	    return 0;
	  }

	/* The order of checking *is* important.  */
	if ((var = is_var_in_list (expr, vars)) != NULL
	    || (var = is_var_in_list (expr, ext_vars)) != NULL)
	  {
	    assert (TREE_TYPE (var) != NULL, 0);
	    TREE_TYPE (expr) = TREE_TYPE (var);
	    TREE_CONSTANT (expr) = TREE_CONSTANT (var);
	  }
	else
	  {
	    error_loc (TREE_LOCATION (expr),
		       "variable `%s' used without previous definition",
		       TREE_STRING_CST (TREE_ID_NAME (expr)));
	    return 1;
	  }
      }
      break;

    /* FIXME: All the code under this case should go to
       a separate function.  */
    case FUNCTION_CALL:
      {
	tree t;
	struct tree_list_element *func_el, *expr_el;
	int func_counter = 0, expr_counter = 0;
	char *  fname = TREE_STRING_CST (TREE_ID_NAME (TREE_OPERAND (expr, 0)));

	/* check if function is declared.  */
	if (!(t = function_exists (fname)))
	  {
	    if (!(t = function_proto_exists (fname)))
	      {
		error_loc (TREE_LOCATION (expr),
			  "function `%s' is not defined", fname);
		return 1;
	      }
	  } 

	/* argument list can be empty.  */
	if (TREE_FUNC_ARG_TYPES (t) != NULL)
	  func_el = TREE_LIST (TREE_FUNC_ARG_TYPES (t));
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
		  return ret;

		expr_counter++;
		if (func_el == NULL)
		  {
		    /* FIXME: Write a helper function for length.  */
		    while (expr_el->next != NULL)
		      {
			expr_counter++;
			expr_el = expr_el->next;
		      }

		    error_loc
		      (TREE_LOCATION (expr),
		       "invalid number of arguments in function `%s' call: "
		       "%u expected, %u found",
		       TREE_STRING_CST (TREE_ID_NAME (TREE_FUNC_NAME (t))),
		       func_counter, expr_counter);
		    return 1;
		  }

		if (!tree_compare (func_el->entry, TREE_TYPE (expr_el->entry)))
		    {
		      /* try to convert types.  */
		      if (conversion_possible (TREE_TYPE (expr_el->entry),
					       func_el->entry))
			{
			  tree t = make_binary_op (CONVERT_EXPR,
						   expr_el->entry,
						   func_el->entry);
			  expr_el->entry = t;
			}
		      else
			{
			  char* func_el_type = tree_to_str (func_el->entry);
			  char* expr_el_type;

			  expr_el_type = tree_to_str (TREE_TYPE (expr_el->entry));
			  error_loc (TREE_LOCATION (expr_el->entry),
				     "argument %u type mismatch: "
				     "`%s' expected, `%s' found",
				     expr_counter, func_el_type, expr_el_type);
			  free (func_el_type);
			  free (expr_el_type);
			  return 1;
			}
		    }
		  func_counter++;
		  func_el = func_el->next;
	      }
	  }
	if (func_el != NULL)
          {
	    /* FIXME: Write a helper function for length.  */
	    while (func_el != NULL)
	      {
	        func_counter++;
	        func_el = func_el->next;
	      }

	    error_loc (TREE_LOCATION (expr),
		       "invalid number of arguments in function '%s' call: "
		       "%u expected, %u found",
		       TREE_STRING_CST (TREE_ID_NAME (TREE_FUNC_NAME (t))),
		       func_counter, expr_counter);
	    return 1;
	  }
	if (TREE_LIST (TREE_FUNC_RET_TYPE (t))->next == NULL)
	  TREE_TYPE (expr) = TREE_LIST (TREE_FUNC_RET_TYPE (t))->entry;
	else
	  TREE_TYPE (expr) = TREE_FUNC_RET_TYPE (t);
	/* We suppose that function calls aren't constant expressions.
	   so we can't predict the return value statically.  */
	TREE_CONSTANT (expr) = false;
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
	    if (conversion_possible (TREE_TYPE (rhs), TREE_TYPE (lhs)))
	      {
		tree t;

		t = make_binary_op (CONVERT_EXPR, rhs, TREE_TYPE (lhs));
		TREE_OPERAND_SET (expr, 1, t);
	      }
	    else
	      {
	 	char* lhs_type = tree_to_str (TREE_TYPE (lhs));
		char* rhs_type = tree_to_str (TREE_TYPE (rhs));
		error_loc (TREE_LOCATION (lhs),
			   "type mismatch. the left operand "
			   "is `%s', the right operand is `%s'",
			   lhs_type, rhs_type);
		free (lhs_type);
		free (rhs_type);
		return 1;
	     }
	  }

	TREE_TYPE (expr) = b_type_node;
	TREE_CONSTANT (expr) = TREE_CONSTANT (lhs) && TREE_CONSTANT (rhs);
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
    	    char* lhs_type = tree_to_str (TREE_TYPE (lhs));
	    char* rhs_type = tree_to_str (TREE_TYPE (rhs));

	    error_loc (TREE_LOCATION (lhs), "expected boolean expressions. "
		       "The left operand is `%s', the right operand is `%s'.",
		       lhs_type, rhs_type);
	    free (lhs_type);
	    free (rhs_type);
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


	/* division's result is always real number.  */
	if (TREE_CODE (expr) != DIV_EXPR)
	  {
	    if (!tree_compare (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	      {
		/* try to convert types.  */
		if (conversion_possible (TREE_TYPE (rhs), TREE_TYPE (lhs)))
		  {
		    tree t;
		    t = make_binary_op (CONVERT_EXPR, rhs, TREE_TYPE (lhs));
		    TREE_OPERAND_SET (expr, 1, t);
		  }
		/* we allow to multiply matrix by a constant (constant has to
		   be the same type as matrix elements.  */
		else if (TREE_CODE (expr) == MULT_EXPR)
		  {
		    if (TYPE_DIM (TREE_TYPE (lhs)) != NULL
		      && TREE_TYPE (rhs) == TREE_TYPE (TREE_LIST 
		      (TREE_LIST (TREE_OPERAND (lhs, 0))->entry)->entry))
		      TREE_TYPE (expr) = TREE_TYPE (lhs);
		    else if (TYPE_DIM (TREE_TYPE (rhs)) != NULL
		      && TREE_TYPE (lhs) == TREE_TYPE (TREE_LIST
			(TREE_LIST (TREE_OPERAND (rhs, 0))->entry)->entry))
		      {
			TREE_TYPE (expr) = TREE_TYPE (rhs);
		      }
		  }
		else
		  {
		    char* lhs_type = tree_to_str (TREE_TYPE (lhs));
		    char* rhs_type = tree_to_str (TREE_TYPE (rhs));

		    error_loc (TREE_LOCATION (lhs),
			       "type mismatch. the left operand "
			       "is `%s', the right operand is `%s'",
			       lhs_type, rhs_type);
		    free (lhs_type);
		    free (rhs_type);
		    return 1;
		  }
	      }
	    if (TREE_TYPE (expr) == NULL)
	      TREE_TYPE (expr) = TREE_TYPE (lhs);
	  }
	else
	  TREE_TYPE (expr) = r_type_node;

	TREE_CONSTANT (expr) = TREE_CONSTANT (lhs) && TREE_CONSTANT (rhs);
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
	TREE_CONSTANT (expr) = TREE_CONSTANT (op);
      }
      break;
    case CIRCUMFLEX:
      {
	tree lhs = TREE_OPERAND (expr, 0);
	tree rhs = TREE_OPERAND (expr, 1);

	ret += typecheck_expression (lhs, ext_vars, vars);
	if (TREE_CIRCUMFLEX_INDEX_STATUS (expr) &&typecheck_options.iter_index)
	  ret += typecheck_recurrent (rhs);
	else
	  ret += typecheck_expression (rhs, ext_vars, vars);

	if (ret != 0 || TREE_TYPE (lhs) == NULL || TREE_TYPE (rhs) == NULL)
	  return 1;

	/* This is a recurrent variable.  */
	if (TREE_CIRCUMFLEX_INDEX_STATUS (expr))
	  {
	    if (TREE_TYPE (rhs) != z_type_node)
	      {
		char* z_type = tree_to_str (z_type_node);
		char* rhs_type = tree_to_str (TREE_TYPE (rhs));
		error_loc (TREE_LOCATION (lhs),
			   "index of a recurrent variable "
			   "must be '%s`, not `%s'",
			   z_type, rhs_type);
		free (z_type);
		free (rhs_type);
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
	return ret;
      }
      break;

    case LOWER:
      {
	ret += typecheck_lower (expr, ext_vars, vars, false);
	if (ret)
	  return ret;
      }
      break;

    case GENERATOR:
      {
	ret += typecheck_generator (expr, ext_vars, vars);
      }
      break;

    case FILTER_EXPR:
      {
	struct tree_list_element *el, *tmp;
	tree gen = TREE_OPERAND (expr, 1);
      	tree new_scope = make_tree_list ();
	unsigned var_counter = 0;

	tree_list_combine (ext_vars, vars);
	DL_FOREACH (TREE_LIST (TREE_OPERAND (expr, 0)), el)
	  {
	    tree index = NULL;

	    if (TREE_CODE (TREE_OPERAND (el->entry, 1)) == IDENTIFIER)
	      index = is_var_in_list (TREE_OPERAND (el->entry, 1), new_scope);

	    assert (TREE_CODE (el->entry) == CIRCUMFLEX,
		    "only operations with upper indexes are supported in filter");

	    ret += typecheck_expression (TREE_OPERAND (el->entry, 0),
					 ext_vars, new_scope);

	    if (index == NULL)
	      {
		if (TREE_CODE (TREE_OPERAND (el->entry, 1)) == IDENTIFIER)
		  {
		    index = TREE_OPERAND (el->entry, 1);
		    tree_list_append (new_scope, index);
		  }
		else
		  ret += typecheck_expression (index, ext_vars, new_scope);

		TREE_TYPE (index) = z_type_node;
	      }

	    TREE_TYPE (el->entry) = TREE_TYPE (TREE_OPERAND (el->entry, 0));
	    var_counter++;
	  }
	TREE_TYPE (expr) = TREE_TYPE (TREE_OPERAND (expr, 0));

	ret += typecheck_generator (gen, ext_vars, new_scope);

	/* if we filter multiple variables, the type of filter expression will
	   be the list of types.  */
	if (var_counter == 1)
	  TREE_TYPE (expr) = TREE_TYPE (TREE_LIST (TREE_OPERAND (expr, 0))->entry);
	else
	  {
	    TREE_TYPE (expr) = make_tree_list ();
	    DL_FOREACH (TREE_LIST (TREE_OPERAND (expr, 0)), el)
	      tree_list_append (TREE_TYPE (expr), TREE_TYPE (el->entry));
	  }

	DL_FOREACH_SAFE (TREE_LIST (new_scope), el, tmp)
	  {
	    DL_DELETE (TREE_LIST (new_scope), el);
	    free (el);
	  }
	free_tree (new_scope);

	/* split combined lists back.  */
	tree_list_split (ext_vars, vars);
	return ret;
     }
      break;

    /* FIXME: Put the code under this case in a separate function.  */
    case GENAR_EXPR:
      {
	tree lim = TREE_OPERAND (expr, 0);
	tree exp = TREE_OPERAND (expr, 1);
	enum tree_code code;
	size_t size;
	tree dim = NULL;
	tree shape = NULL;
	ret += typecheck_expression (lim, ext_vars, vars);
	ret += typecheck_expression (exp, ext_vars, vars);

	if (ret)
	  return ret;
	code = TREE_CODE (TREE_TYPE (exp));
	size = TYPE_SIZE (TREE_TYPE (exp));

	if ((TREE_CODE (TREE_TYPE (lim)) != Z_TYPE
	     && TREE_CODE (TREE_TYPE (lim)) != N_TYPE)
	    || (TYPE_DIM (TREE_TYPE (lim)) != NULL
		&& TREE_INTEGER_CST (TYPE_DIM (TREE_TYPE (lim))) != 1))
	  {
	    error_loc (TREE_LOCATION (expr),
		       "boundary for 'genar' expression must be either "
		       "integer or vector");
	    ret += 1;
	  }
	if (TYPE_DIM (TREE_TYPE (lim)) == NULL)
	  {
	    if (TYPE_DIM (TREE_TYPE (exp)) == NULL)
	      {
		dim = make_integer_cst (1);
		if (TREE_CODE (lim) == INTEGER_CST)
		  {
		    shape = make_tree_list ();
		    tree_list_append (shape,
				      make_integer_cst (TREE_INTEGER_CST (lim)));
		  }
		else
		  /* there is lack of information to draw
		     conclusions about the shape.  */
		  shape = unknown_mark_node;
	      }
	    else
	      {
		dim = make_integer_cst (1
		  + TREE_INTEGER_CST (TYPE_DIM (TREE_TYPE (exp))));
		if (TYPE_SHAPE (TREE_TYPE (exp)) != NULL)
		  {
		    struct tree_list_element *el;
		    shape = make_tree_list ();
		    tree_list_append (shape,
			make_integer_cst (TREE_INTEGER_CST (lim)));
		    DL_FOREACH (TREE_LIST (TYPE_SHAPE (TREE_TYPE (exp))),
							    el)
		      {
			tree_list_append (shape, make_integer_cst
			    (TREE_INTEGER_CST (el->entry)));
		      }
		  }
	      }
	  }
	else
	  {
	    if (TYPE_DIM (TREE_TYPE (exp)) == NULL)
		dim = make_integer_cst ( TREE_INTEGER_CST (
		      TREE_LIST (TYPE_SHAPE (TREE_TYPE (lim)))->entry));
	    else
	      {
		dim = make_integer_cst (TREE_INTEGER_CST (
		      TREE_LIST (TYPE_SHAPE (TREE_TYPE (lim)))->entry)
		      + TREE_INTEGER_CST (TYPE_DIM (TREE_TYPE (exp))));

	      }
	    if (TREE_CODE (lim) == MATRIX_EXPR)
	      {
		struct tree_list_element *el, *tel;
		shape = make_tree_list ();

		DL_FOREACH (TREE_LIST (TREE_OPERAND (lim, 0)), tel)
		  {
		    DL_FOREACH (TREE_LIST (tel->entry), el)
		      {
			if (TREE_CODE (el->entry) == INTEGER_CST)
			  tree_list_append (shape, tree_copy (el->entry));
			else
			  {
			    free_tree (shape);
			    shape = unknown_mark_node;
			    break;
			  }
		      }
		  }
		if (TYPE_SHAPE (TREE_TYPE (exp)) != NULL
		 && shape != unknown_mark_node)
		  {
		    DL_FOREACH (TREE_LIST (TYPE_SHAPE (TREE_TYPE (exp))), el)
		      {
			if (TREE_CODE (el->entry) == INTEGER_CST)
			  tree_list_append (shape, tree_copy (el->entry));
			else
			  {
			    free_tree (shape);
			    shape = unknown_mark_node;
			    break;
			  }
		      }
		  }
	      }
	    else
	      shape = unknown_mark_node;
	  }

	TREE_TYPE (expr) = types_assign_type (code, size, dim, shape);

	if (dim != TYPE_DIM (TREE_TYPE (expr)))
	  free_tree (dim);
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
	bool const_matrix = true;
	tree shape_t, dim_t;

	DL_FOREACH (TREE_LIST (el_list), l)
	  {
	    shape_x = 0;
	    DL_FOREACH (TREE_LIST (l->entry), el)
	      {
		ret += typecheck_expression (el->entry, ext_vars, vars);

		if (!TREE_CONSTANT (el->entry))
		    const_matrix = false;

		if  (TYPE_DIM (TREE_TYPE (el->entry)) != NULL
		     || TYPE_SHAPE (TREE_TYPE (el->entry)) != NULL)
		  {
		    error_loc (TREE_LOCATION (el->entry),
			       "vector types are not supported in "
			       "arrays at the moment");
		    ret += 1;
		  }

		if (!code)
		  code = TREE_CODE (TREE_TYPE (el->entry));
		else if (TREE_CODE (TREE_TYPE (el->entry)) != code)
		  {
		    char* el_type = tree_to_str (TREE_TYPE (el->entry));

		    error_loc (TREE_LOCATION (el->entry),
			       "type mismatch: `%s' expected, `%s' found",
			       TREE_CODE_NAME (code), el_type);
		    free (el_type);
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

	tree_list_append (shape_t, make_integer_cst (shape_y));
	if (shape_x != 1)
	  tree_list_append (shape_t, make_integer_cst (shape_x));

	TREE_TYPE (expr) = types_assign_type (code, size, dim_t, shape_t);
	TREE_CONSTANT (expr) = const_matrix;

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
