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

#define DEF_TREE_CODE(code, desc, class, operands, typed) class,
enum tree_code_class tree_code_type[] =
{
#include "tree.def"
};

#undef DEF_TREE_CODE

#define DEF_TREE_CODE(code, desc, class, operands, typed) operands,
unsigned char tree_code_operand[] = {
#include "tree.def"
};

#undef DEF_TREE_CODE

#define DEF_TREE_CODE(code, desc, class, operands, typed) typed,
bool tree_code_typed[] = {
#include "tree.def"
};

#undef DEF_TREE_CODE

#define DEF_TREE_CODE(code, desc, class, operands, typed) desc,
const char *tree_code_name[] = {
#include "tree.def"
};

#undef DEF_TREE_CODE

/* Table to store tree nodes that should be allocated only once.  */
tree global_tree[TG_MAX];

/* When freeing a tree node atomic objects like
   identifiers and string constants can have multiple links.
   That is why when freeing an atomic object we change the code
   of the tree to EMPTY_MARK and save this tree int the
   atomic_trees.  */
static tree *  atomic_trees = NULL;
static size_t  atomic_trees_size = 0;
static size_t  atomic_trees_idx = 0;

tree
tree_init (enum tree_code code, size_t * size)
{
  if (TREE_CODE_TYPED (code))
    {
      if (TREE_CODE_OPERANDS (code) > 0)
	return (tree) malloc (*size = sizeof (struct tree_type_base_op));
      else
	return (tree) malloc (*size = sizeof (struct tree_type_base));
    }
  else
    {
      if (TREE_CODE_OPERANDS (code) > 0)
	return (tree) malloc (*size = sizeof (struct tree_base_op));
      else
	return (tree) malloc (*size = sizeof (struct tree_base));
    }
  return NULL;
}

tree
make_tree (enum tree_code code)
{
  tree ret;
  size_t size, ops;
  
  ops = TREE_CODE_OPERANDS (code) * sizeof (tree);
  size = TREE_CODE_TYPED (code) 
	 && (ops ? sizeof (struct tree_base_op)
		 : sizeof (struct tree_base))
	 || (ops ? sizeof (struct tree_type_base_op)
		 : sizeof (struct tree_type_base));
  
  switch (TREE_CODE_CLASS (code))
    {
    case tcl_misc:
      if (code == IDENTIFIER)
	ret = (tree) malloc (size = ops + sizeof (struct tree_identifier_node));
      else if (code == LIST)
	ret = (tree) malloc (size = ops + sizeof (struct tree_list_node));
      else if (code == ERROR_MARK)
	{
	  warning ("attempt to allocate ERRO_MARK_NODE; pointer returned");
	  return error_mark_node;
	}
      else
	ret = (tree) malloc (size = size + ops);
      break;

    case tcl_type:
      ret = (tree) malloc (size = ops + sizeof (struct tree_type_node));
      break;

    case tcl_constant:
      if (code == INTEGER_CST)
	ret = (tree) malloc (size = ops + sizeof (struct tree_int_cst_node));
      else if (code == STRING_CST)
	ret = (tree) malloc (size = ops + sizeof (struct tree_string_cst_node));
      else
	unreachable (0);
      break;
    case tcl_expression:
      if (code == CIRCUMFLEX)
	ret = (tree) malloc (size = sizeof (struct tree_circumflex_op_node));
      else
	ret = (tree) malloc (size = size + ops);
      break;

    default:
      unreachable (0);
      break;
    }
  
  memset (ret, 0, size);
  TREE_CODE_SET (ret, code);
  return ret;
}


static inline void
atomic_trees_add (tree t)
{
  assert (TREE_CODE (t) == EMPTY_MARK,
	  "only EMPTY_MARK nodes can be added to atomic_tres");

  if (atomic_trees_size == 0)
    {
      const size_t initial_size = 32;

      atomic_trees = (tree *) malloc (initial_size * sizeof (tree));
      atomic_trees_size = initial_size;
    }

  if (atomic_trees_idx == atomic_trees_size)
    {
      atomic_trees
	= (tree *) realloc (atomic_trees,
			    2 * atomic_trees_size * sizeof (tree));
      atomic_trees_size *= 2;
    }

  /* Most likely we don't need to search anything.  */

  {				/* For testing purposes only.  */
    size_t i;
    for (i = 0; i < atomic_trees_idx; i++)
      if (atomic_trees[i] == t)
	unreachable ("double insert of node in atomic_trees");
  }

  atomic_trees[atomic_trees_idx++] = t;
  //printf ("-- atomix_idx = %i\n", (int)atomic_trees_idx);
}


void
free_atomic_trees ()
{
  size_t i;

  for (i = 0; i < atomic_trees_idx; i++)
    {
      assert (TREE_CODE (atomic_trees[i]) == EMPTY_MARK, 0);
      free (atomic_trees[i]);
    }

  if (atomic_trees)
    free (atomic_trees);

  atomic_trees = NULL;
  atomic_trees_size = 0;
  atomic_trees_idx = 0;
}


void
free_tree (tree node)
{
  int i;
  enum tree_code code;

  if (node == NULL
      || node == error_mark_node || TREE_CODE (node) == EMPTY_MARK)
    return;

  code = TREE_CODE (node);
  /*switch (TREE_CODE_CLASS (TREE_CODE (node)))
    {

    case tcl_misc:
      if (code == IDENTIFIER)
	{
	  free_tree (TREE_ID_NAME (node));
	  TREE_ID_NAME (node) = NULL;
	  TREE_CODE_SET (node, EMPTY_MARK);
	}
      else if (code == LIST)
	{
	  utarray_free (TREE_LIST (node));
	}
      else
	unreachable (0);
      break;

    case tcl_type:
      free_tree (TREE_TYPE_DIM (node));
      TREE_TYPE_DIM (node) = NULL;
      free_tree (TREE_TYPE_SHAPE (node));
      TREE_TYPE_SHAPE (node) = NULL;
      TREE_CODE_SET (node, EMPTY_MARK);
      break;

    case tcl_constant:
      if (code == INTEGER_CST)
	{
	  TREE_CODE_SET (node, EMPTY_MARK);
	}
      else if (code == STRING_CST)
	{
	  if (TREE_STRING_CST (node))
	    free (TREE_STRING_CST (node));
	  TREE_CODE_SET (node, EMPTY_MARK);
	}
      else
	unreachable (0);
      break;

    case tcl_expression:
      if (code == UMINUS_EXPR || code == NOT_EXPR)
	break;
      else
	break;
      break;

    default:
      unreachable (0);
      break;
    }

  for (i = 0; i < TREE_CODE_OPERANDS (code); i++)
    {
      free_tree (TREE_OPERAND (node, i));
      TREE_OPERAND_SET (node, i, NULL);
    }

  if (TREE_CODE (node) == EMPTY_MARK)
    atomic_trees_add (node);
  else
    free (node);*/
}

tree
make_function (tree name, tree args, tree args_types, tree ret, tree instrs,
	       struct location loc)
{
  tree t = make_tree (FUNCTION);
  TREE_OPERAND_SET (t, 0, name);
  TREE_OPERAND_SET (t, 1, args);
  TREE_OPERAND_SET (t, 2, args_types);
  TREE_OPERAND_SET (t, 3, ret);
  TREE_OPERAND_SET (t, 4, instrs);
  TREE_LOCATION (t) = loc;
  return t;
}

tree
make_string_cst_str (const char *value)
{
  tree t;

  assert (value != NULL, 0);

  t = make_tree (STRING_CST);
  TREE_STRING_CST (t) = strdup (value);
  TREE_STRING_CST_LENGTH (t) = strlen (value);
  /* FIXME Add is_char modifier to the tree.  */

  return t;
}

tree
make_string_cst_tok (struct token * tok)
{
  tree t;
  const char *str;

  assert (token_class (tok) == tok_id
	  || token_class (tok) == tok_keyword,
	  "attempt to build sting_cst from %s",
	  token_class_as_string (token_class (tok)));

  str = token_as_string (tok);
  t = make_string_cst_str (str);
  TREE_LOCATION (t) = token_location (tok);

  return t;
}

tree
make_identifier_tok (struct token * tok)
{
  tree t;
  assert (is_id (tok, false),
	  "attempt to build identifier from %s",
	  token_class_as_string (token_class (tok)));

  t = make_tree (IDENTIFIER);
  TREE_ID_NAME (t) = make_string_cst_tok (tok);
  TREE_LOCATION (t) = token_location (tok);
  return t;
}

tree
make_tree_list ()
{
  tree t = make_tree (LIST);
	TREE_LIST(t) = NULL;
  return t;
}

tree
make_integer_tok (struct token * tok)
{
  tree t;
  assert (token_class (tok) == tok_number,
	  "attempt to build number from %s",
	  token_class_as_string (token_class (tok)));

  t = make_tree (INTEGER_CST);
  TREE_INTEGER_CST (t) = atoi (token_as_string (tok));
  TREE_LOCATION (t) = token_location (tok);
  return t;
}

bool
tree_list_append (tree list, tree elem)
{
  assert (TREE_CODE (list) == LIST, 
          "appending element of type `%s'", TREE_CODE_NAME (TREE_CODE (list)));
  
  DL_APPEND (TREE_LIST(list), elem);
	return true;
}

tree
make_type (enum tree_code code)
{
  tree t;
  assert (TREE_CODE_CLASS (code) == tcl_type,
	  "%s called with %s tree code", __func__, TREE_CODE_NAME (code));
  t = make_tree (code);
  return t;
}

tree
make_binary_op (enum tree_code code, tree lhs, tree rhs)
{
  tree t;

  //printf ("-- %s enter with code %s\n", __func__, TREE_CODE_NAME (code));
  assert (TREE_CODE_CLASS (code) == tcl_expression
	  && code != UMINUS_EXPR && code != NOT_EXPR,
	  "%s called with %s tree code", __func__, TREE_CODE_NAME (code));
  t = make_tree (code);
  TREE_OPERAND_SET (t, 0, lhs);
  TREE_OPERAND_SET (t, 1, rhs);
  if (lhs != NULL)
    TREE_LOCATION (t) = TREE_LOCATION (lhs);
  return t;
}

tree
make_matrix (tree format, tree list, struct location loc)
{
  tree t;
  t = make_tree (MATRIX_EXPR);
  TREE_OPERAND_SET (t, 0, format);
  TREE_OPERAND_SET (t, 1, list);
  TREE_LOCATION (t) = loc;
  return t;
}

tree
make_vector (tree list, struct location loc)
{
  tree t;
  t = make_tree (VECTOR_EXPR);
  TREE_OPERAND_SET (t, 0, list);
  TREE_LOCATION (t) = loc;
  return t;
}

tree
make_genar (tree a, tree b, struct location loc)
{
  tree t;
  t = make_tree (GENAR_EXPR);
  TREE_OPERAND_SET (t, 0, a);
  TREE_OPERAND_SET (t, 1, b);
  TREE_LOCATION (t) = loc;
  return t;
}

tree
make_return (tree a, struct location loc)
{
  tree t = make_tree (RETURN_EXPR);
  TREE_OPERAND_SET (t, 0, a);
  TREE_LOCATION (t) = loc;
  return t;
}

tree
make_with_loop (tree idx, tree cond, tree expr, bool flag)
{
  tree t = make_tree (WITH_LOOP_EXPR);
  TREE_OPERAND_SET (t, 0, idx);
  TREE_OPERAND_SET (t, 1, cond);
  TREE_OPERAND_SET (t, 2, expr);
  TREE_LOCATION (t) = TREE_LOCATION (idx);
  return t;
}

tree
make_unary_op (enum tree_code code, tree val, struct location loc)
{
  tree t;
  assert (TREE_CODE_CLASS (code) == tcl_expression
	  && (code == UMINUS_EXPR || code == NOT_EXPR),
	  "%s called with %s tree code", __func__, TREE_CODE_NAME (code));
  t = make_tree (code);
  TREE_OPERAND_SET (t, 0, val);
  TREE_LOCATION (t) = loc;
  return t;

}

tree
make_assign (enum token_kind tk, tree lhs, tree rhs)
{
  assert (is_assignment_operator (tk),
	  "attempt to make assignment from %s", token_kind_as_string (tk));

  //printf ("-- %s enter\n", __func__);
  switch (tk)
    {
    case tv_gets:
      return make_binary_op (ASSIGN_EXPR, lhs, rhs);
    default:
      unreachable ("assignment creation failed");
    }

  return error_mark_node;
}

tree
tree_list_copy (tree lst)
{
  tree cpy;

  if (lst == NULL)
    return NULL;

  assert (TREE_CODE (lst) == LIST,
	  "cannot copy list from %s", TREE_CODE_NAME (TREE_CODE (lst)));

  cpy = make_tree_list ();
	DL_CONCAT (TREE_LIST(cpy), TREE_LIST(lst));
  return cpy;
}


/* This function frees the list structure without touching
   any tree pointers that list stores. It is useful to destroy
   intermediate lists.  */
void
free_list (tree lst)
{
	
}
