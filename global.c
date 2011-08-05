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


/* FIXME do we want to store standard types here as well?
   Table that stores user-defined types.  */
tree type_list = NULL;

/* Here we would like to store all the constants
   defined outside the functions and expands. As an
   example consider strlist construction.  */
tree constant_list = NULL ;

/* A global list to store functions and expands.  */
tree function_list = NULL;

/* A global list to store function prototypes. 
   Function prototype is a function name and a list of types.  */
tree function_proto_list = NULL;

/* Allocat all the global structures that are going to be used
   during the compilation.  */
void 
init_global ()
{
  assert (type_list == NULL, "type list is already allocated");
  assert (constant_list == NULL, "constant list is already allocated");
  assert (function_list == NULL, "function list is already allocated");
  
  type_list = make_tree (LIST);
  TAILQ_INIT (&TREE_LIST_QUEUE (type_list));

  constant_list = make_tree (LIST);
  TAILQ_INIT (&TREE_LIST_QUEUE (constant_list));
  
  function_list = make_tree (LIST);
  TAILQ_INIT (&TREE_LIST_QUEUE (function_list));

  function_proto_list = make_tree (LIST);
  TAILQ_INIT (&TREE_LIST_QUEUE (function_proto_list));

  error_count = 0;
  warning_count = 0;
}

void
finalize_global ()
{
  free_tree (type_list);
  free_tree (constant_list);
  free_tree (function_list);
  free_tree (function_proto_list);
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
  return t == INTEGER_TYPE || t == STRING_TYPE
         || t == LIST_TYPE || t == VOID_TYPE
         || t == USER_TYPE;
}

tree
make_function_proto_args (int num, va_list params)
{
  tree t = make_tree_list ();
  tree arg;
  int i;

  for (i = 0; i < num; i++)
    {
      tree type;
      arg = va_arg (params, tree);
      assert (is_valid_type (arg), 
              "Attempt to use non-type node in the function prototype");

      type = type_defined (TREE_STRING_CST (TREE_TYPE_NAME (arg)));
      if (type == NULL)
        {
          error ("Undefined type `%s' in function prototype",
                 TREE_STRING_CST (TREE_TYPE_NAME (arg)));
          free_list (t);
          return NULL;
        }
      tree_list_append (t, type);
    }

  return t;
}

bool
type_lists_eq (tree tal, tree tar)
{     
  struct tree_list_element *  lptr;
  struct tree_list_element *  rptr;

  assert (TREE_CODE (tal) == LIST 
          && TREE_CODE (tar) == LIST, 0);

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

  return true;
}

tree
function_proto_exists (tree name, tree ret, tree args)
{
  struct tree_list_element *  tel;

  assert (TREE_CODE (name) == STRING_CST, 0);
  assert (is_valid_type (ret), 0);
  assert (TREE_CODE (args) == LIST, 0);

  TAILQ_FOREACH (tel, &TREE_LIST_QUEUE (function_proto_list), entries)
    {
      tree _name = TREE_OPERAND (tel->element, 0);
      tree _ret  = TREE_OPERAND (tel->element, 1);
      tree _args = TREE_OPERAND (tel->element, 2);

      bool names_eq = strcmp (TREE_STRING_CST (name), 
                              TREE_STRING_CST (_name)) == 0;
      bool types_eq = _ret == ret;
      bool args_eq = type_lists_eq (_args, args);
     
      if (names_eq && types_eq && args_eq)
        return tel->element;
    }

  return NULL;
}

tree
make_function_proto (tree name, tree ret_type, int num_args, ...)
{
  tree t, fargs, ret;
  va_list vl;

  assert (TREE_CODE (name) == STRING_CST, "Invalid function name");
  ret = type_defined (TREE_STRING_CST (TREE_TYPE_NAME (ret_type)));
  if (ret == NULL)
    {
      error ("Undefined function return type `%s'",
             TREE_STRING_CST (TREE_TYPE_NAME (ret_type)));
      return NULL;
    }

  va_start (vl, num_args);
  fargs = make_function_proto_args (num_args, vl);
  va_end (vl);

  if (fargs == NULL)
    return NULL;
  
  if (function_proto_exists (name, ret, fargs))
    {
      /* FIXME Yeah, bogus error-message, I know.  */
      error ("Function prototype exists for function `%s'",
             TREE_STRING_CST (name));
      return NULL;
    }
  
  t = make_tree (FUNCTION_PROTO);
  TREE_OPERAND_SET (t, 0, name);
  TREE_OPERAND_SET (t, 1, ret);
  TREE_OPERAND_SET (t, 2, fargs);

  return t;
}

void
init_function_protos ()
{
#define MAKE_PROTO(name, ret, num, ...) \
  do { \
    tree __name, __proto; \
    __name = make_string_cst_str (name); \
    __proto = make_function_proto (__name, ret, num, __VA_ARGS__); \
    assert (__proto != NULL, "Error creating function prototype `%s'", name); \
    tree_list_append (function_proto_list, __proto); \
  } while (0)

  MAKE_PROTO ("str", string_type_node, 1, integer_type_node);
  MAKE_PROTO ("str", string_type_node, 1, list_type_node);
  MAKE_PROTO ("length", integer_type_node, 1, list_type_node);
  MAKE_PROTO ("length", integer_type_node, 1, string_type_node);
  MAKE_PROTO ("index", string_type_node, 2, string_type_node, integer_type_node);
  /* XXX How can we do an index of LIST?   
         Most likely we will have to introduce internal type ANY
         to describe an element of generic list.  */
  MAKE_PROTO ("assert", integer_type_node, 1, integer_type_node);
  MAKE_PROTO ("generate", void_type_node, 1, string_type_node);
}

void
finalize_function_protos ()
{
}

/* Function returns tree if the type exists in the 
   table or NULL otherwise.  */
tree 
type_defined (const char *  name)
{
  struct tree_list_element *  tel;
  
  assert (type_list != NULL, "user types are not allocated");
  /* assert (TREE_CODE (name) == STRING_CST, "type name must be string"); */

  TAILQ_FOREACH (tel, &TREE_LIST_QUEUE (type_list), entries)
    {
      /*printf ("-- %s, %s\n", TREE_STRING_CST (TREE_USER_TYPE_NAME (tel->element)), name);*/
      if (strcmp (TREE_STRING_CST (TREE_TYPE_NAME (tel->element)), name) == 0)
        return tel->element;
    }
  return NULL;
}


/* Adds type named NAME to the user-defined type table USER_TYPES.  */
tree 
add_user_type (tree name)
{
  struct tree_list_element *tel;
  tree t;

  assert (TREE_CODE (name) == STRING_CST, "user-type name must be a string");
  t = type_defined (TREE_STRING_CST (name));
  if (t != NULL)
    {
      warning ("type redefined");
      return error_mark_node;
    }

  tel = (struct tree_list_element *) malloc (sizeof (struct tree_list_element));
  t = make_tree (USER_TYPE);
  TREE_TYPE_NAME (t) = name;
  tel->element = t;

  TAILQ_INSERT_TAIL (&TREE_LIST_QUEUE (type_list), tel, entries);
  return t;
}

tree
expand_exists (const char * str)
{
  struct tree_list_element *  tl;
  
  assert (function_list != NULL, "function-list is not initialized");

  TAILQ_FOREACH (tl, &TREE_LIST_QUEUE (function_list), entries)
    {
      if (TREE_CODE (tl->element) != EXPAND_STMT)
        continue;
      
      if (strcmp (TREE_STRING_CST (TREE_OPERAND (tl->element, 0)), str) == 0)
        return tl->element;
    }

  return NULL;
}

tree
function_exists (const char * str)
{
  struct tree_list_element *  tl;
  
  assert (function_list != NULL, "function-list is not initialized");

  TAILQ_FOREACH (tl, &TREE_LIST_QUEUE (function_list), entries)
    {
      if (TREE_CODE (tl->element) != FUNCTION_STMT)
        continue;
      
      if (strcmp (TREE_STRING_CST (TREE_OPERAND (tl->element, 0)), str) == 0)
        return tl->element;
    }

  return NULL;
}


/* XXX Currently we support only strlist constants.  */
tree
constant_exists (const char * str)
{
  struct tree_list_element *  tl;
  
  assert (constant_list != NULL, "function-list is not initialized");

  TAILQ_FOREACH (tl, &TREE_LIST_QUEUE (constant_list), entries)
    {
      tree t;
      assert (TREE_CODE (tl->element) == ASSIGN_EXPR, 
              "Constant should be defined using assign_expr");
      
      t = TREE_OPERAND (tl->element, 0);
      assert (TREE_CODE (t) == IDENTIFIER, 0);

      if (strcmp (TREE_STRING_CST (TREE_ID_NAME (t)), str) == 0)
        return tl->element;
    }

  return NULL;

}
