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

#include "tree.h"
#include "global.h"
#include "print.h"
#include "types.h"

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
static tree *atomic_trees = NULL;
static size_t atomic_trees_size = 0;
static size_t atomic_trees_idx = 0;


/* Convert tree into it's string representation.
   NOTE This string is your resposibility to clean up.  */
char*
tree_to_str (tree t)
{
  char *  ret;
  xfile *  xf = xfile_init_memory (16);

  print_expression (xf, t);
  ret = XFILE_BUFFER (xf);
  free (xf);

  return ret;
}

size_t
get_tree_size (enum tree_code code)
{
  size_t size, ops;
  ops = TREE_CODE_OPERANDS (code) * sizeof (tree);
  size = TREE_CODE_TYPED (code)
	 ? (ops ? sizeof (struct tree_type_base_op)
		: sizeof (struct tree_type_base))
	 : (ops ? sizeof (struct tree_base_op)
		: sizeof (struct tree_base));

  switch (TREE_CODE_CLASS (code))
    {
    case tcl_misc:
      if (code == IDENTIFIER)
	return ops + sizeof (struct tree_identifier_node);
      else if (code == LIST)
	return ops + sizeof (struct tree_list_node);
      else if (code == ITER_EXPR)
	return ops + sizeof (struct tree_rec_expr_node);
      else if (code == ERROR_MARK)
	return 0;
      else
	return size + ops;

    case tcl_type:
      return ops + sizeof (struct tree_type_node);

    case tcl_constant:
      if (code == INTEGER_CST)
	return ops + sizeof (struct tree_int_cst_node);
      else if (code == REAL_CST)
	return ops + sizeof (struct tree_real_cst_node);
      else if (code == STRING_CST)
	return ops + sizeof (struct tree_string_cst_node);
      else
	unreachable (0);
      break;

    case tcl_expression:
      if (code == CIRCUMFLEX)
	return ops + sizeof (struct tree_circumflex_op_node);
      else
	return size + ops;

    case tcl_statement:
      return size + ops;

    default:
      unreachable (0);
      break;
    }

  return 0;
}

tree
make_tree (enum tree_code code)
{
  size_t size = get_tree_size (code);

  if (code == ERROR_MARK)
    warning ("attempt to allocate ERRO_MARK_NODE; pointer returned");

  tree ret = (tree) malloc (size);
  memset (ret, 0, size);
  TREE_CODE_SET (ret, code);

  if (TREE_CODE_TYPED (code))
    TREE_TYPE (ret) = NULL;

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
      atomic_trees =
	(tree *) realloc (atomic_trees,
			  2 * atomic_trees_size * sizeof (tree));
      atomic_trees_size *= 2;
    }

  /* Most likely we don't need to search anything.  */

  {/* For testing purposes only.  */
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
      /* Types are removed separetely.  */
      || node == iter_var_node
      || node == error_mark_node || TREE_CODE (node) == EMPTY_MARK
      || TREE_CODE_CLASS (TREE_CODE (node)) == tcl_type)
    return;

  code = TREE_CODE (node);
  switch (TREE_CODE_CLASS (code))
    {
    case tcl_misc:
      if (code == IDENTIFIER)
	{
	  //tree_list_append (delete_list, TREE_ID_NAME (node));
	  free_tree (TREE_ID_ITER (node));
	  free_tree (TREE_ID_NAME (node));
	  free_tree (TREE_ID_SOURCE_NAME (node));
	}
      else if (code == LIST)
	{
	  struct tree_list_element *el = NULL, *tmp = NULL;
	  DL_FOREACH_SAFE (TREE_LIST (node), el, tmp)
	  {
	    DL_DELETE (TREE_LIST (node), el);
	    free_tree (el->entry);
	    free (el);
	  }
	}
      else if (code == ITER_EXPR)
	{
	  free_tree (TREE_ITER_LIST (node));
	}
      break;

    case tcl_constant:
      if (code == STRING_CST)
	{
	  if (TREE_STRING_CST (node))
	    free (TREE_STRING_CST (node));
	}
      else if (code == INTEGER_CST)
	{

	}
      else if (code == REAL_CST)
	{

	}
      else
	unreachable (0);
      break;

    case tcl_expression:
      break;

    case tcl_statement:
      break;
    default:
      unreachable (0);
      break;
    }

  /* Types are stored in global hash table. That's why we delete them
     separetely.  */
  if (TREE_CODE_TYPED (code))
    TREE_TYPE (node) = NULL;
  for (i = 0; i < TREE_CODE_OPERANDS (code); i++)
    {
      free_tree (TREE_OPERAND (node, i));
      TREE_OPERAND_SET (node, i, NULL);
    }

  TREE_CODE_SET (node, EMPTY_MARK);
  atomic_trees_add (node);
}

void
free_tree_type (tree node, bool hard)
{
  enum tree_code code;
  if (node == NULL)
    return;
  code = TREE_CODE (node);
  assert (TREE_CODE_CLASS (code) == tcl_type,
	  "Only types are allowed to be deleted in free_tree_type function");

  if (code == FUNCTION_TYPE)
    {
      if (hard)
	{
	  if (TYPE_FUNCTION_ARGS (node))
	    {
	      if (TREE_CODE_CLASS (TREE_CODE (
		    TYPE_FUNCTION_ARGS (node))) != tcl_type)
		free_tree (TYPE_FUNCTION_ARGS (node));
	      else
		free_tree_type (TYPE_FUNCTION_ARGS (node), true);
	    }

	  if (TYPE_FUNCTION_RET (node))
	    {
	      if (TREE_CODE_CLASS (TREE_CODE (
		    TYPE_FUNCTION_RET (node))) != tcl_type)
		free_tree (TYPE_FUNCTION_RET (node));
	      else
		free_tree_type (TYPE_FUNCTION_RET (node), true);
	    }
	  assert (TREE_CODE (TYPE_LIST (node)) == LIST, 
		"a list property code of `%s' has to be `%s' only",
		TREE_CODE_NAME (FUNCTION_TYPE),
		TREE_CODE_NAME (LIST));
	  free_tree (TYPE_LIST (node));
	}
      else
	{
	  tree_list_append (delete_list, TYPE_FUNCTION_ARGS (node));
	  tree_list_append (delete_list, TYPE_FUNCTION_RET (node));
	}
      TYPE_FUNCTION_ARGS (node) = NULL;
      TYPE_FUNCTION_RET (node) = NULL;
      TYPE_LIST (node) = NULL;
    }
  else
    {
      /* if hard is not set we don't free dim and shape.  */
      if (hard)
	{
	  if (TYPE_DIM (node) != NULL)
	    {
	      if (TREE_CODE_CLASS (TREE_CODE (TYPE_DIM (node))) != tcl_type)
		free_tree (TYPE_DIM (node));
	      else
		free_tree_type (TYPE_DIM (node), true);
	    }

	  if (TYPE_SHAPE (node) != NULL)
	    {
	      if (TREE_CODE_CLASS (TREE_CODE (TYPE_SHAPE (node))) != tcl_type)
		free_tree (TYPE_SHAPE (node));
	      else
		free_tree_type (TYPE_SHAPE (node), true);
	    }
	}
      TYPE_DIM (node) = NULL;
      TYPE_SHAPE (node) = NULL;
    }
  TREE_CODE_SET (node, EMPTY_MARK);
  atomic_trees_add (node);
}

tree
make_function (tree name, tree args, tree arg_types, tree ret, tree instrs,
	       struct location loc)
{
  tree t = make_tree (FUNCTION);
  TREE_OPERAND_SET (t, 0, name);
  TREE_OPERAND_SET (t, 1, args);
  TREE_OPERAND_SET (t, 2, arg_types);
  TREE_OPERAND_SET (t, 3, ret);
  TREE_OPERAND_SET (t, 4, instrs);
  TREE_LOCATION (t) = loc;
  return t;
}

tree
make_string_cst_str (const char *value)
{
  tree t, type;

  assert (value != NULL, 0);
  assert (TREE_CODE_TYPED (STRING_CST), "string has to have a type");

  t = make_tree (STRING_CST);
  TREE_STRING_CST (t) = strdup (value);
  TREE_STRING_CST_LENGTH (t) = strlen (value);
  TREE_CONSTANT (t) = true;
  if (strlen (value) == 1)
    TREE_STRING_CST_IS_CHAR (t) = true;
  type = make_type (STRING_TYPE);
  TYPE_SIZE (type) = (strlen (value) + 1) * 8;
  TREE_TYPE (t) = types_assign_type (type);
  if (TREE_TYPE (t) != type)
    free_tree_type (type, true);
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
  assert (is_id (tok, false), "attempt to build identifier from %s",
	  token_class_as_string (token_class (tok)));

  if (token_value (tok) == tv_iter)
    return iter_var_node;

  t = make_tree (IDENTIFIER);
  TREE_ID_NAME (t) = make_string_cst_tok (tok);
  /* `source name' is used to be outputed in error messages, as
     variable names are going to be modified by compiler.  */
  TREE_ID_SOURCE_NAME (t) = make_string_cst_tok (tok);
  TREE_ID_DEFINED (t) = false;
  TREE_ID_ITER (t) = NULL;
  TREE_LOCATION (t) = token_location (tok);
  return t;
}

tree
make_tree_list ()
{
  tree t = make_tree (LIST);
  TREE_LIST (t) = NULL;
  return t;
}

tree
make_integer_cst (int value)
{
  tree t = make_tree (INTEGER_CST);
  TREE_INTEGER_CST (t) = value;
  TREE_CONSTANT (t) = true;
  TREE_TYPE (t) = z_type_node;
  return t;
}

tree
make_integer_tok (struct token * tok)
{
  tree t;
  assert (token_class (tok) == tok_intnum,
	  "attempt to build integer number from %s",
	  token_class_as_string (token_class (tok)));

  t = make_tree (INTEGER_CST);
  TREE_INTEGER_CST (t) = atoll (token_as_string (tok));
  TREE_CONSTANT (t) = true;
  assert (TREE_CODE_TYPED (INTEGER_CST), "real number has to have a type");
  TREE_TYPE (t) = z_type_node;
  TREE_LOCATION (t) = token_location (tok);
  return t;
}

tree
make_real_tok (struct token * tok)
{
  tree t;
  assert (token_class (tok) == tok_realnum,
	  "attempt to build real number from %s",
	  token_class_as_string (token_class (tok)));

  t = make_tree (REAL_CST);
  TREE_REAL_CST (t) = atof (token_as_string (tok));
  TREE_CONSTANT (t) = true;
  assert (TREE_CODE_TYPED (REAL_CST), "real number has to have a type");
  TREE_TYPE (t) = r_type_node;
  TREE_LOCATION (t) = token_location (tok);
  return t;
}

/* Combine two lists into one.  There is no memory allocation, just pointer
   reallocation.
   NOTE You need both of *left* and *right* pointers to split these lists
   again.  */
void
tree_list_combine (tree left, tree right)
{
  struct tree_list_element *tmp;
  if (TREE_LIST (right) == NULL)
    return;
  if (TREE_LIST (left) != NULL)
    {
      tmp = TREE_LIST (right)->prev;
      TREE_LIST (right)->prev = TREE_LIST (left)->prev;
      TREE_LIST (left)->prev->next = TREE_LIST (right);
      TREE_LIST (left)->prev = tmp;
    }
  else
    TREE_LIST (left) = TREE_LIST (right);
  return;
}

/* Split a combined list.
   NOTE It doesn't check if the *right* is a part of the *left* list.  So be
   careful to use it and check it's affilation by yourself.  */
void
tree_list_split (tree left, tree right)
{
  if (TREE_LIST (right) == NULL)
    return;
  if (TREE_LIST (left) != TREE_LIST (right))
    {
      struct tree_list_element *tmp = TREE_LIST(left)->prev;
      TREE_LIST(left)->prev = TREE_LIST (right)->prev;
      TREE_LIST (right)->prev->next = NULL;
      TREE_LIST (right)->prev = tmp;
    }
  else
    TREE_LIST (left) = NULL;
  return;
}


bool
tree_list_append (tree list, tree elem)
{
  struct tree_list_element *el;
  assert (TREE_CODE (list) == LIST, "appending element of type `%s'",
	  TREE_CODE_NAME (TREE_CODE (list)));

  el =
    (struct tree_list_element *) malloc (sizeof (struct tree_list_element));

  assert (el != NULL, "Can't allocate enough memory for new element `%s'",
	  TREE_CODE_NAME (TREE_CODE (list)));
  el->entry = elem;

  DL_APPEND (TREE_LIST (list), el);
  return true;
}

tree
make_type (enum tree_code code)
{
  tree t = NULL;
  assert (TREE_CODE_CLASS (code) == tcl_type, "%s called with %s tree code",
	  __func__, TREE_CODE_NAME (code));

  t = make_tree (code);
  if (TREE_CODE (t) == B_TYPE)
    TYPE_SIZE (t) = 1;
  else if (TREE_CODE (t) == N_TYPE)
    TYPE_SIZE (t) = 8 * sizeof (unsigned);
  else if (TREE_CODE (t) == Z_TYPE)
    TYPE_SIZE (t) = 8 * sizeof (int);
  else if (TREE_CODE (t) == R_TYPE)
    TYPE_SIZE (t) = 8 * sizeof (double);
  TYPE_SHAPE (t) = NULL;
  TYPE_DIM (t) = NULL;

  return t;
}

tree
make_binary_op (enum tree_code code, tree lhs, tree rhs)
{
  tree t;
  assert ((TREE_CODE_CLASS (code) == tcl_expression && code != UMINUS_EXPR
	  && code != NOT_EXPR)
	  || code == ITER_PAIR
	  || code == DECLARE_STMT
	  || code == ASSIGN_STMT
	  , "%s called with %s tree code", __func__,
	  TREE_CODE_NAME (code));

  t = make_tree (code);
  TREE_OPERAND_SET (t, 0, lhs);
  TREE_OPERAND_SET (t, 1, rhs);

  if (lhs != NULL)
    TREE_LOCATION (t) = TREE_LOCATION (lhs);
  /* FIXME else what? no location??  */

  return t;
}

tree
make_matrix (tree list, struct location loc)
{
  tree t;
  t = make_tree (MATRIX_EXPR);
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
  tree t = make_tree (RETURN_STMT);
  TREE_OPERAND_SET (t, 0, a);
  TREE_LOCATION (t) = loc;
  return t;
}

tree
make_index_loop (tree idx, tree cond, tree expr, bool flag)
{
  tree t = make_tree (INDEX_LOOP_EXPR);
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
	  "%s called with %s tree code", __func__,
	  TREE_CODE_NAME (code));
  t = make_tree (code);
  TREE_OPERAND_SET (t, 0, val);
  TREE_LOCATION (t) = loc;
  return t;

}

tree
make_assign (enum token_kind tk, tree lhs, tree rhs)
{
  assert (is_assignment_operator (tk), "attempt to make assignment from %s",
	  token_kind_as_string (tk));

  switch (tk)
    {
    case tv_gets:
      return make_binary_op (ASSIGN_STMT, lhs, rhs);
    default:
      unreachable ("assignment creation failed");
    }

  return error_mark_node;
}

tree
make_convert (tree from, tree to)
{
  tree t = make_binary_op (CONVERT_EXPR, from, to);
  TREE_TYPE (t) = to;
  return t;
}

tree
tree_list_copy (tree lst)
{
  tree cpy;
  struct tree_list_element *el;
  if (lst == NULL)
    return NULL;

  assert (TREE_CODE (lst) == LIST, "cannot copy list from %s",
	  TREE_CODE_NAME (TREE_CODE (lst)));

  cpy = make_tree_list ();

  DL_FOREACH (TREE_LIST (lst), el)
    tree_list_append (cpy, tree_copy (el->entry));
  return cpy;
}

tree
tree_copy (tree t)
{
  tree tmp;
  int i = 0;

  if (TREE_CODE (t) == LIST)
    {
      tmp = tree_list_copy (t);
      return tmp;
    }

  tmp = make_tree (TREE_CODE (t));
  memcpy (tmp, t, get_tree_size (TREE_CODE (t)));

  switch (TREE_CODE (t))
    {
    case (STRING_CST):
      TREE_STRING_CST (tmp) = strdup (TREE_STRING_CST (t));
      break;
    case (IDENTIFIER):
      TREE_ID_NAME (tmp) = tree_copy (TREE_ID_NAME (t));
      if (TREE_ID_ITER (t) != NULL)
	TREE_ID_ITER (tmp) = tree_copy (TREE_ID_ITER (t));
      break;
    default:
      break;
    }

  for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE (t)); i++)
    TREE_OPERAND_SET (tmp, i, tree_copy (TREE_OPERAND (t, i)));

  return tmp;
}

/* This function frees the list structure without touching
   any tree pointers that list stores. It is useful to destroy
   intermediate lists.  */
void
free_list (tree lst)
{
  struct tree_list_element *ptr;
  struct tree_list_element *tmpptr;

  if (lst == NULL)
    return;

  ptr = TREE_LIST (lst);
  while (ptr != NULL)
    {
      tmpptr = ptr->next;
      if (ptr)
	free (ptr);
      ptr = tmpptr;
    }
  free (lst);
}

bool
tree_compare (tree left, tree right)
{
  struct tree_list_element *lel = NULL, *rel = NULL;
  int i;

  if (left == right)
    return true;

  if (left == NULL)
    {
      if (right == NULL)
	return true;
      else
	return false;
    }

  if (right == NULL)
    return false;

  if (left == error_mark_node)
    return right == error_mark_node;

  if (TREE_CODE (left) != TREE_CODE (right))
    return false;

  if (TREE_CODE (left) == LIST)
    {
      /* compare lists.  */
      for (lel = TREE_LIST (left), rel = TREE_LIST (right); lel && rel;
	   lel = lel->next, rel = rel->next)
	{
	  if (!tree_compare (lel->entry, rel->entry))
	    return false;
	}
      if (rel != lel)
	return false;
      return true;
    }

  /* comparision of typed nodes.  */
  if (TREE_CODE_TYPED (TREE_CODE (left)))
    {
      if (!tree_compare (TREE_TYPE (left), TREE_TYPE (right)))
	return false;

      if (left->typed.argset != right->typed.argset
	  || left->typed.arg != right->typed.arg
	  || left->typed.is_constant != right->typed.is_constant)
	return false;
    }

  /* types comparision.  */
  if (TREE_CODE_CLASS (TREE_CODE (left)) == tcl_type)
    return TYPE_SIZE (left) == TYPE_SIZE (right)
	   && tree_compare (TYPE_DIM (left), TYPE_DIM (right))
	   && tree_compare (TYPE_SHAPE (left), TYPE_SHAPE (right));

  /* circumflex comparision.  */
  if (TREE_CODE (left) == CIRCUMFLEX)
    return TREE_CIRCUMFLEX_INDEX_STATUS (left)
	   == TREE_CIRCUMFLEX_INDEX_STATUS (right);

  if (TREE_CODE (left) == STRING_TYPE)
    return TREE_STRING_CST_LENGTH (left) == TREE_STRING_CST_LENGTH (right)
	   && !strcmp (TREE_STRING_CST (left), TREE_STRING_CST (right));

  if (TREE_CODE (left) == INTEGER_CST)
    return TREE_INTEGER_CST (left) == TREE_INTEGER_CST (right);

  if (TREE_CODE (left) == REAL_CST)
    return TREE_REAL_CST (left) == TREE_REAL_CST (right);

  if (TREE_CODE (left) == IDENTIFIER)
    return tree_compare (TREE_ID_NAME (left), TREE_ID_NAME (right));

  /* Operand comparision.  */
  for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE (left)); i++)
    if (!tree_compare (TREE_OPERAND (left, i), TREE_OPERAND (right, i)))
      return false;

  return true;
}

tree eliminate_list (tree expr)
{
  tree tmp = expr;
  assert (TREE_CODE (expr) == LIST, "list tree expected");
  expr = TREE_LIST (expr)->entry;
  free_list (tmp);
  return expr;
}

int equal_list_sizes (tree left, tree right)
{
  struct tree_list_element *lel = NULL, *rel =NULL;
  assert (TREE_CODE (left) == LIST && TREE_CODE (right) == LIST,
    "list tree expected");
  DL_FOREACH (TREE_LIST (left), lel)
    {
      if (rel == NULL)
	rel = TREE_LIST (right);
      
      if (lel->next == NULL && rel->next != NULL)
	return -1;
      else if (lel->next != NULL && rel->next == NULL)
	return 1;
      
      rel = rel->next;
    }
  return 0;
}
