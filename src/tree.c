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
static tree *  atomic_trees = NULL;
static size_t  atomic_trees_size = 0;
static size_t  atomic_trees_idx = 0;

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
      free(atomic_trees[i]);
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
      || TREE_CODE_CLASS (TREE_CODE (node)) == tcl_type
      || node == error_mark_node || TREE_CODE(node) == EMPTY_MARK)
    return;

  code = TREE_CODE (node);

  switch (TREE_CODE_CLASS (code))
    {
    case tcl_misc:
      if (code == IDENTIFIER)
	{
	  free_tree (TREE_ID_NAME (node));
	}
      else if (code == LIST)
	{
	  struct tree_list_element * el = NULL, *tmp = NULL;
	  DL_FOREACH_SAFE(TREE_LIST(node), el, tmp)
	    {
	      DL_DELETE (TREE_LIST (node), el);
	      free_tree (el->entry);
	      free (el);
	    }
	}
      break;

    case tcl_constant:
      if (code == STRING_CST)
	{
	  if (TREE_STRING_CST (node))
	    free (TREE_STRING_CST (node));
	}
      else
      if (code == INTEGER_CST)
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

void free_tree_type (tree node)
{
  enum tree_code code;
  if (node == NULL)
    return;
  code  = TREE_CODE (node);
  assert (TREE_CODE_CLASS (code) == tcl_type,
    "Only types are allowed to be deleted in free_tree_type function");

  if (TYPE_DIM (node) != NULL)
    {
      if (TREE_CODE_CLASS (TREE_CODE (TYPE_DIM (node))) != tcl_type)
	free_tree (TYPE_DIM (node));
      else
	free_tree_type (TYPE_DIM (node));

      TYPE_DIM (node) = NULL;
    }

  if (TYPE_SHAPE (node) != NULL)
    {
      if (TREE_CODE_CLASS (TREE_CODE (TYPE_SHAPE (node))) != tcl_type)
	free_tree (TYPE_SHAPE (node));
      else
	free_tree_type (TYPE_SHAPE (node));

      TYPE_SHAPE (node) = NULL;
    }

  TREE_CODE_SET (node, EMPTY_MARK);
  atomic_trees_add (node);
}

tree
make_function (tree name, tree args, tree args_types,
	       tree ret, tree instrs, struct location loc)
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
  assert (TREE_CODE_TYPED (STRING_CST), "string has to have a type");

  t = make_tree (STRING_CST);
  TREE_STRING_CST (t) = strdup (value);
  TREE_STRING_CST_LENGTH (t) = strlen (value);
  TREE_TYPE (t) =
      types_assign_type (STRING_TYPE, ((strlen (value) + 1) * 8), NULL, NULL);
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
make_integer_cst (int value)
{
  tree t = make_tree (INTEGER_CST);
  TREE_INTEGER_CST (t) = value;
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
  TREE_INTEGER_CST (t) = atoi (token_as_string (tok));
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
  assert (TREE_CODE_TYPED (REAL_CST), "real number has to have a type");
  TREE_TYPE (t) = r_type_node;
  TREE_LOCATION (t) = token_location (tok);
  return t;
}

bool
tree_list_append (tree list, tree elem)
{
  struct tree_list_element * el;
  assert (TREE_CODE (list) == LIST,
          "appending element of type `%s'", TREE_CODE_NAME (TREE_CODE (list)));

  el = (struct tree_list_element *) malloc (sizeof(struct tree_list_element));

  assert (el != NULL, "Can't allocate enough memory for new element `%s'",
  TREE_CODE_NAME(TREE_CODE(list)));
  el->entry = elem;

  DL_APPEND (TREE_LIST(list), el);
  return true;
}

tree
make_type (enum tree_code code)
{
  tree t = NULL;
  assert (TREE_CODE_CLASS (code) == tcl_type,
	  "%s called with %s tree code", __func__, TREE_CODE_NAME (code));
  t = make_tree (code);
  if (TREE_CODE (t) == B_TYPE)
    TYPE_SIZE (t) = 8;
  else if (TREE_CODE (t) == N_TYPE)
    TYPE_SIZE (t) = 8 * sizeof (unsigned);
  else if (TREE_CODE (t) == Z_TYPE)
    TYPE_SIZE (t) = 8 * sizeof (int);
  else if (TREE_CODE (t) == R_TYPE)
    TYPE_SIZE (t) = 8 * sizeof (double);
  TYPE_SHAPE(t) = NULL;
  TYPE_DIM(t) = NULL;
  return t;
}

tree
make_binary_op (enum tree_code code, tree lhs, tree rhs)
{
  tree t;

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
  struct tree_list_element * el;
  if (lst == NULL)
    return NULL;

  assert (TREE_CODE (lst) == LIST,
	  "cannot copy list from %s", TREE_CODE_NAME (TREE_CODE (lst)));

  cpy = make_tree_list ();

  DL_FOREACH (TREE_LIST(lst), el)
    tree_list_append(cpy, tree_copy (el->entry));
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

  tmp = make_tree (TREE_CODE(t));
  memcpy (tmp, t, get_tree_size (TREE_CODE(t)));

  switch (TREE_CODE(t))
    {
      case (STRING_CST):
	tmp->string_cst_node.value = strdup (t->string_cst_node.value);
	break;
      case (IDENTIFIER):
	tmp->identifier_node.name = tree_copy (t->identifier_node.name);
	break;
      default:
	break;
    }

  for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE(t)); i++)
      TREE_OPERAND_SET(tmp, i, tree_copy (TREE_OPERAND(t, i)));

  return tmp;
}

/* This function frees the list structure without touching
   any tree pointers that list stores. It is useful to destroy
   intermediate lists.  */
void
free_list (tree lst)
{
  struct tree_list_element * ptr;
  struct tree_list_element * tmpptr;

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

/* Get some data from the tree to use it for hashing.  */
#define FORMAT_DATA(data, value) \
do { \
  int step = 1;	\
  while (step <= value) \
    step *= 10; \
  data = data * step + value; \
} while (0)
UT_array*
tree_get_hash_data (tree t, UT_array* arr)
{
  int i = 0;
  int data = 0;
  if (t == NULL)
    {
      utarray_push_back (arr, &data);
      return arr;
    }

  data = (int) TREE_CODE (t) + 1;

  switch (TREE_CODE (t))
    {
      case IDENTIFIER:
	FORMAT_DATA (data, strlen(TREE_STRING_CST (TREE_ID_NAME (t))));
	break;
      case LIST:
	{
	  struct tree_list_element *el = NULL;
	  DL_FOREACH (TREE_LIST (t), el)
	    {
	      utarray_push_back (arr, &data);
	      tree_get_hash_data (el->entry, arr);
	    }
	  return arr;
	}
      case INTEGER_CST:
	FORMAT_DATA (data, TREE_INTEGER_CST (t));
	break;
      case REAL_CST:
	FORMAT_DATA (data, (int) TREE_REAL_CST (t));
	break;
      case STRING_CST:
	FORMAT_DATA (data, strlen (TREE_STRING_CST (t)));
	break;
      default:
	break;
    }
  utarray_push_back (arr, &data);
  for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE (t)); i++)
    tree_get_hash_data (TREE_OPERAND (t, i), arr);

  return arr;
}
#undef FORMAT_DATA

bool
tree_compare (tree left, tree right)
{
  struct tree_list_element *lel = NULL, *rel = NULL;
  int i;
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
      for (lel = TREE_LIST(left), rel = TREE_LIST(right);
	    lel && rel; lel=lel->next, rel=rel->next)
	{
	  if (!tree_compare (lel->entry, rel->entry))
	    return false;
	}
      return true;
    }

  /* comparision of typed nodes.  */
  if (TREE_CODE_TYPED(TREE_CODE (left)))
    {
      if (!tree_compare (TREE_TYPE (left), TREE_TYPE (right)))
	return false;

      if  (left->typed.argset != right->typed.argset
	|| left->typed.arg != right->typed.arg
	|| left->typed.is_constant != right->typed.is_constant)
	return false;
    }

  /* types comparision.  */
  if (TREE_CODE_CLASS (TREE_CODE (left)) == tcl_type)
    {
      if (TYPE_SIZE (left) != TYPE_SIZE (right))
	return false;

      if (!tree_compare (TYPE_DIM (left), TYPE_DIM (right)))
	return false;

      if (!tree_compare (TYPE_SHAPE (left), TYPE_SHAPE (right)))
	return false;
    }

  /* circumflex comparision.  */
  if (TREE_CODE (left) == CIRCUMFLEX)
    {
      if (   TREE_CIRCUMFLEX_INDEX_STATUS (left)
	  != TREE_CIRCUMFLEX_INDEX_STATUS (right))
	return false;
    }


  if (TREE_CODE (left) == STRING_TYPE)
    {
      if (TREE_STRING_CST_LENGTH (left) != TREE_STRING_CST_LENGTH (right))
	return false;

      if (strcmp(TREE_STRING_CST (left), TREE_STRING_CST (right)))
	return false;
    }

  if (TREE_CODE (left) == INTEGER_CST)
    {
      if (TREE_INTEGER_CST (left) != TREE_INTEGER_CST (right))
	return false;
    }

  if (TREE_CODE (left) == REAL_CST)
    {
      if (TREE_REAL_CST (left) != TREE_REAL_CST (right))
	return false;
    }

  if (TREE_CODE (left) == IDENTIFIER)
    {
      if (!tree_compare (TREE_ID_NAME (left), TREE_ID_NAME (right)))
	return false;
    }

  /* Operand comparision.  */
  for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE (left)); i++)
    {
      if (!tree_compare (TREE_OPERAND (left, i), TREE_OPERAND (right, i)))
	return false;
    }

    return true;

}
