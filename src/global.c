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
#include "expand.h"
#include "tree.h"
#include "global.h"


/* Variable that is going to be increased every 
   time when an error is happening.  */
int error_count = 0;

/* Variable that is going to be increased every 
   time when an error is happening.  */
int warning_count = 0;

/* A global list to store functions.  */
tree function_list = NULL;

/* A global lists to store types.  */
tree type_name_list = NULL;
tree type_size_list = NULL;

/* Allocate all the global structures that are going to be used
   during the compilation.  */
void 
init_global ()
{
  assert (function_list == NULL, "function list is already allocated");

  function_list = make_tree_list ();
  type_name_list = make_tree_list ();
  type_size_list = make_tree_List ();

  error_count = 0;
  warning_count = 0;
}

void
add_primitive_type_names ()
{

}

void 
add_primitive_type_sizes ()
{

}

void
finalize_global ()
{
  free_tree (function_list);
}

void
init_global_tree ()
{
  global_tree[TG_ERROR_MARK] = (tree) malloc (sizeof (struct tree_base));
  TREE_CODE_SET (global_tree[TG_ERROR_MARK], ERROR_MARK);

#define MAKE_TYPE(tg_id, code, tok_kind) \
  do { \
    tree __t; \
    global_tree[tg_id] = (tree) malloc (sizeof (struct tree_type_node)); \
    TREE_CODE_SET (global_tree[tg_id], code); \
    __t = make_string_cst_str (token_kind_as_string (tok_kind)); \
    TREE_TYPE_NAME (global_tree[tg_id]) = __t; \
    tree_list_append (type_list, global_tree[tg_id]); \
  } while (0)
  
  /* Here we assume that we have only one kind of integers.  */
  /* MAKE_TYPE (TG_INTEGER_TYPE, INTEGER_TYPE, tv_int);
  MAKE_TYPE (TG_STRING_TYPE, STRING_TYPE, tv_str);
  MAKE_TYPE (TG_LIST_TYPE, LIST_TYPE, tv_list); */
  /* FIXME currently we don't know what to do with this keyword
     when it appears.  */
  /* MAKE_TYPE (TG_VOID_TYPE, VOID_TYPE, tv_void); */
}

void
finalize_global_tree ()
{
  int i;
  for (i = 0; i < TG_MAX; i++)
    if (global_tree[i] == error_mark_node)
      free (global_tree[i]);
    else
      free_tree (global_tree[i]);
}


static inline bool
is_valid_type (tree type)
{
  enum tree_code t;

  if (type == NULL)
    return false;

  t = TREE_CODE (type);
  return t == B_TYPE || t == N_TYPE
         || t == Z_TYPE || t == R_TYPE;
}

bool
type_lists_eq (tree tal, tree tar)
{     
/*  tree *  lptr = NULL;
  tree *  rptr = NULL;

  assert (TREE_CODE (tal) == LIST 
          && TREE_CODE (tar) == LIST, 0);

	while ( (lptr = (tree*) utarray_next (TREE_LIST(tal), lptr)) &&
					(rptr = (tree*) utarray_next (TREE_LIST(tal), rptr)))
	{
		if (lpt
	}
	lptr = TAILQ_FIRST (&TREE_LIST_QUEUE (tal));
  TAILQ_FOREACH (rptr, &TREE_LIST_QUEUE (tar), entries)
    {
      if (lptr->element != rptr->element)
        return false;

      if ((TAILQ_NEXT (lptr, entries) == NULL) 
          != (TAILQ_NEXT (rptr, entries) == NULL))
        return false;

      lptr = TAILQ_NEXT (lptr, entries);
    }
*/
  return true;
}

tree
function_exists (const char * str)
{
  struct tree_list_element *  tl;
  
  assert (function_list != NULL, "function-list is not initialized");

	DL_FOREACH (TREE_LIST(function_list), tl)
	{
		if (strcmp (TREE_STRING_CST (TREE_OPERAND (tl->entry, 0)), str) == 0)
			return tl->entry;
	}

  return NULL;
}
