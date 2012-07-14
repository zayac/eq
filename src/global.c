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
#include <stdarg.h>
#include <string.h>
#include "types.h"
#include "eq.h"
#include "tree.h"
#include "uthash.h"
#include "global.h"
#include "types.h"

/* Variable that is going to be increased every
   time when an error is happening.  */
int error_count = 0;

/* Variable that is going to be increased every
   time when an error is happening.  */
int warning_count = 0;

/* A global list to store functions.  */
tree function_list = NULL;

/* A global list to store function prototypes.
   Function prototype is a function name, a list of argument types and the
   return value type.  */
tree function_proto_list = NULL;

/* Trees that are to be deleted.  */
tree delete_list = NULL;

/* A list of iterable variables.  */
tree iter_var_list = NULL;

/* A list of streams.  */
tree stream_list = NULL;

/* Allocate all the global structures that are going to be used
   during the compilation.  */
void
init_global ()
{
  assert (function_list == NULL, "function list is already allocated");

  function_list = make_tree_list ();
  function_proto_list = make_tree_list ();
  stream_list = make_tree_list ();
  iter_var_list = make_tree_list ();
  delete_list = make_tree_list ();
  error_count = 0;
  warning_count = 0;
}

void
finalize_global ()
{
  free_tree (delete_list);
  free_tree (iter_var_list);
  free_tree (stream_list);
  free_tree (function_proto_list);
  free_tree (function_list);
}

void
init_global_tree ()
{
  global_tree[TG_ERROR_MARK] = (tree) malloc (sizeof (struct tree_base));
  global_tree[TG_UNKNOWN_MARK] = (tree) malloc (sizeof (struct tree_base));
  global_tree[TG_ITER_VAR] = (tree) malloc (sizeof (struct tree_identifier_node));
  
  TREE_ID_NAME (global_tree[TG_ITER_VAR])
      = make_string_cst_str (token_kind_name[tv_iter]);
  TREE_ID_DEFINED (global_tree[TG_ITER_VAR]) = true;
  TREE_ID_ITER (global_tree[TG_ITER_VAR]) = NULL;

  TREE_CODE_SET (global_tree[TG_ERROR_MARK], ERROR_MARK);
  TREE_CODE_SET (global_tree[TG_UNKNOWN_MARK], UNKNOWN_MARK);
  TREE_CODE_SET (global_tree[TG_ITER_VAR], IDENTIFIER);

  types_init ();
  /* we can assign types only after defined types.  */
  TREE_TYPE (global_tree[TG_ITER_VAR]) = z_type_node;
}

void
finalize_global_tree ()
{
  int i;
  types_finalize ();
  for (i = 0; i < TG_MAX; i++)
    if (global_tree[i] == error_mark_node)
      free (global_tree[i]);
    else if (global_tree[i] == iter_var_node)
      {
	free_tree (TREE_ID_ITER (global_tree[i]));
	free_tree (TREE_ID_NAME (global_tree[i]));
	free (global_tree[i]);
      }
    else
      free_tree (global_tree[i]);
}

int
compare_ints (const void *a, const void *b)
{
  const int *ia = (const int *) a;
  const int *ib = (const int *) b;
  return (*ia > *ib) - (*ia < *ib);
}

static inline bool
is_valid_type (tree type)
{
  enum tree_code t;

  if (type == NULL)
    return false;

  t = TREE_CODE (type);
  return t == B_TYPE || t == N_TYPE || t == Z_TYPE || t == R_TYPE;
}

static inline tree
function_or_proto_exists (tree list, const char *str)
{
  struct tree_list_element *tl;

  assert (list != NULL, "function-list or proto-list is not initialized");

  DL_FOREACH (TREE_LIST (list), tl)
    {
      if (strcmp (TREE_STRING_CST (TREE_ID_NAME ((TREE_OPERAND (tl->entry, 0)))),
		str) == 0)
	return tl->entry;
    }

  return NULL;
}

tree
function_exists (const char *str)
{
  return function_or_proto_exists (function_list, str);
}

tree
function_proto_exists (const char *str)
{
  return function_or_proto_exists (function_proto_list, str);
}

inline tree
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

inline tree
is_int_in_list (tree var, tree lst)
{
  struct tree_list_element *tle;
  tree ret = NULL;


  assert (TREE_CODE (lst) == LIST, "Integer list expected");
  assert (TREE_CODE (var) == INTEGER_CST, "Integer expected");

  if (lst == NULL)
    return NULL;

  /* NOTE we *must* return the last match value  at this point
     because the list could be concatenation of two variable
     lists of nested blocks. */
  DL_FOREACH (TREE_LIST (lst), tle)
  {
    if (TREE_INTEGER_CST (var) == TREE_INTEGER_CST (tle->entry))
      ret = tle->entry;
  }

  return ret;
}
