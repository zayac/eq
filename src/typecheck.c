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


int
typecheck ()
{
  struct tree_list_element *  tl;
  int function_check = 0;

  DL_FOREACH (TREE_LIST (function_list), tl)
    function_check += typecheck_function (tl->element);

  return const_check + function_check;
}

int
typecheck_constant (tree cst)
{
  tree lhs, rhs;
  int ret = 0;

  assert (TREE_CODE (cst) == ASSIGN_EXPR 
          || TREE_CODE (cst) == IDENTIFIER, 0);

  if (TREE_CODE (cst) == IDENTIFIER)
    {
      if (TREE_TYPE (cst) == NULL ||
          type_defined (TREE_STRING_CST (TREE_TYPE_NAME (TREE_TYPE (cst))))
          == NULL)
        {
          error_loc (TREE_LOCATION (cst),
                     "Constant of unknown type found");
          return 1;
        }

      return 0;
    }

  /* XXX At the time being we assume that the constants could
     be only a list of string expressions. Seriously:
        varname = ["str", "str", ...]  */
  lhs = TREE_OPERAND (cst, 0);
  rhs = TREE_OPERAND (cst, 1);

  if (TREE_TYPE (rhs) == NULL)
    ret = typecheck_expression (rhs, NULL, NULL);

  if (TREE_TYPE (rhs) != list_type_node || !TREE_CONSTANT (rhs))
    {
      error_loc (TREE_LOCATION (rhs), 
                 "constant list expected in the assignment");
      return 1;
    }

  if (TREE_TYPE (lhs) == NULL)
    COPY_TYPE_AND_ATTRS (lhs, rhs);
  else if (!TYPE_AND_ATTRS_EQ (lhs, rhs))
    {
      error_loc (TREE_LOCATION (lhs), "types in the assignment do not match");
      return 1;
    }

  return ret;
}

int
typecheck_stmt_list (tree stmt_list, tree ext_vars)
{
  struct tree_list_element *  tel;
  int ret = 0;

  assert (TREE_CODE (stmt_list) == STMT_LIST, "statement list expected");

  if (TREE_STMT_LIST_VARS (stmt_list) == NULL)
    TREE_STMT_LIST_VARS (stmt_list) = make_tree_list ();

  TAILQ_FOREACH (tel, &TREE_LIST_QUEUE (TREE_STMT_LIST_STMTS (stmt_list)),
                 entries)
    {
      ret += typecheck_stmt (tel->element, 
                             ext_vars,
                             TREE_STMT_LIST_VARS (stmt_list));
    }
  return ret;
}

static inline tree
is_var_in_list (tree var, tree lst)
{
  struct tree_list_element *  tel;
  
  assert (TREE_CODE (lst) == LIST, "Variable list expected");
  assert (TREE_CODE (var) == IDENTIFIER, "Variable expected");

  if (lst == NULL)
    return NULL;

  /* NOTE we *must* perform a reverse search at this point
     because the list could be concatenation of two variable
     lists of nested blocks.  */ 
  TAILQ_FOREACH_REVERSE (tel, &TREE_LIST_QUEUE (lst), tree_list, entries)
    {
      //fprintf (stderr, "-- var %s\n", TREE_STRING_CST (TREE_ID_NAME (var)));
      if (strcmp (TREE_STRING_CST (TREE_ID_NAME (var)),
                  TREE_STRING_CST (TREE_ID_NAME (tel->element))) == 0)
        return tel->element;
    }

  return NULL;
}

static inline tree
is_var_in_consts (tree var)
{
  struct tree_list_element *  tel;
  
  assert (TREE_CODE (var) == IDENTIFIER, "Variable expected");

  if (constant_list == NULL)
    return NULL;

  TAILQ_FOREACH (tel, &TREE_LIST_QUEUE (constant_list), entries)
    {
      tree cst_var = TREE_CODE (tel->element) == ASSIGN_EXPR
                     ? TREE_OPERAND (tel->element, 0)
                     : tel->element;

      if (strcmp (TREE_STRING_CST (TREE_ID_NAME (var)),
                  TREE_STRING_CST (TREE_ID_NAME (cst_var))) == 0)
        return cst_var;
    }

  return NULL;
}



int
typecheck_stmt (tree stmt, tree ext_vars, tree vars)
{
  int ret = 0;

  switch (TREE_CODE (stmt))
    {
#if 0    
    case CALL_EXPR:
      {
         tree name_tree = TREE_OPERAND (stmt, 0);
         const char *  name = TREE_STRING_CST (name_tree);
         
         if (strcmp (name, "generate") == 0)
           {
             ret = typecheck_expression (TREE_OPERAND (stmt, 1), ext_vars, vars);
             if (TREE_TYPE (stmt) != list_type_node)
               {
                 /* FIXME Insert implicit conversion here.  */
                 error_loc (TREE_LOCATION (stmt), 
                            "generate argument must be of type string");
                 return 1;
               }
             return ret;
           }
         else if (strcmp (name, "assert") == 0)
           {
             ret = typecheck_expression (TREE_OPERAND (stmt, 1), ext_vars, vars);
             if (TREE_TYPE (stmt) != integer_type_node)
               {
                 /* FIXME Insert implicit conversion here.  */
                 error_loc (TREE_LOCATION (stmt),
                            "assert argument must be of type int");
                 return 1;
               }
             return ret;
           }
         else 
           return typecheck_expression (stmt, ext_vars, vars);
      }
      break;
#endif
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
            tree_list_append (vars, lhs);
          }

        /* FIXME Check that these types are not NULL.  */
        if (TREE_TYPE (lhs) == NULL)
          COPY_TYPE_AND_ATTRS (lhs, rhs);
        else if (!TYPE_AND_ATTRS_EQ (lhs, rhs))
          {
            error_loc (TREE_LOCATION (lhs),
                       "Assignment left hand side type does not "
                       "match right hand side type");
            return 1;
          }
        return 0;
      }
      break;
    case IF_STMT:
      {
        tree cond = TREE_OPERAND (stmt, 0);
        tree if_branch, else_branch;
        tree res;

        struct tree_list_element *  vars_first;
        struct tree_list_element *  ext_last;
        //struct tree_list_element *  tel;


        ret = typecheck_expression (cond, ext_vars, vars);
        if (TREE_TYPE (cond) !=  integer_type_node)
          {
            error_loc (TREE_LOCATION (cond),
                       "Condition must be of type integer");
            return 1;
          }
        
        /*TAILQ_FOREACH (tel, &TREE_LIST_QUEUE (vars), entries)
          {       
            int i = 0;
            print_expression (stderr, tel->element);
            fprintf (stderr, " %i\n", i++);
          }        
        
        TAILQ_FOREACH (tel, &TREE_LIST_QUEUE (ext_vars), entries)
          {
            int i = 10;
            print_expression (stderr, tel->element);
            fprintf (stderr, " %i\n", i++);
          }*/
 
        
        /* ext_vars + vars, just to check statement list.  */
        vars_first = TAILQ_FIRST (&TREE_LIST_QUEUE (vars));
        ext_last   = TAILQ_LAST (&TREE_LIST_QUEUE (ext_vars), tree_list);
        
        if (ext_last == NULL)
          res = vars;
        else if (vars_first == NULL)
          res = ext_vars;
        else
          {
            struct tree_list * ext_tl = &TREE_LIST_QUEUE (ext_vars);
            struct tree_list * vars_tl = &TREE_LIST_QUEUE (vars);
            
            assert (TAILQ_NEXT (ext_last, entries) == NULL
                    && TAILQ_PREV (vars_first, tree_list, entries) == NULL, 0);

            *ext_tl->tqh_last = vars_tl->tqh_first;
            vars_tl->tqh_first->entries.tqe_prev = ext_tl->tqh_last;
            ext_tl->tqh_last = vars_tl->tqh_last;
            res = ext_vars;
          }
        
        assert ((if_branch = TREE_OPERAND (stmt, 1)) != NULL, 0);
        ret += typecheck_stmt_list (if_branch, res);

        else_branch = TREE_OPERAND (stmt, 2);
        if (else_branch != NULL)
          ret += typecheck_stmt_list (else_branch, res);

        if (ext_last != NULL && vars_first != NULL)
          {
            struct tree_list * ext_tl = &TREE_LIST_QUEUE (ext_vars);

            ext_last->entries.tqe_next = NULL;
            ext_tl->tqh_last = &ext_last->entries.tqe_next;
            *vars_first->entries.tqe_prev = NULL;
          }
        /*TAILQ_FOREACH (tel, &TREE_LIST_QUEUE (ext_vars), entries)
          {
            int i = 110;
            print_expression (stderr, tel->element);
            fprintf (stderr, " %i\n", i++);
          }

        TAILQ_FOREACH (tel, &TREE_LIST_QUEUE (vars), entries)
          {       
            int i = 100;
            print_expression (stderr, tel->element);
            fprintf (stderr, " %i\n", i++);
          }*/
 
      }
      break;
    case FOR_STMT:
      break;
    case C_CALL_EXPR:
      break;
    default:
      assert (TREE_CODE_CLASS (TREE_CODE (stmt)) == tcl_expression,
              "expression expected");
      return typecheck_expression (stmt, ext_vars, vars);
    }

  return ret;
}

int
typecheck_function (tree func)
{
  return 0;
}

static tree
type_list_from_args (tree args, tree ext_vars, tree vars)
{
  tree type_list = make_tree_list ();
  struct tree_list_element *  tel;

  TAILQ_FOREACH (tel, &TREE_LIST_QUEUE (args), entries)
    {
      int ret = 0;
      
      if (TREE_TYPE (tel->element) == NULL)
        ret = typecheck_expression (tel->element, ext_vars, vars);

      if (ret != 0)
        {
          free_list (type_list);
          return NULL;
        }

      tree_list_append (type_list, TREE_TYPE (tel->element));
    }

  return type_list;
}

static tree
match_proto_name_args (tree name, tree args)
{
  struct tree_list_element *  tel;

  assert (TREE_CODE (name) == STRING_CST
          && TREE_CODE (args) == LIST, 0);
  
  TAILQ_FOREACH (tel, &TREE_LIST_QUEUE (function_proto_list), entries)
    {
      if (strcmp (TREE_STRING_CST (name), 
                  TREE_STRING_CST (TREE_OPERAND (tel->element, 0))) == 0
          && type_lists_eq (TREE_OPERAND (tel->element, 2), args))
        return tel->element;
    }

  return NULL;
}

static tree
match_proto_name_ret_args (tree name, tree ret, tree args)
{
  struct tree_list_element *  tel;

  assert (TREE_CODE (name) == STRING_CST
          && TREE_CODE (args) == LIST, 0);
  TAILQ_FOREACH (tel, &TREE_LIST_QUEUE (function_proto_list), entries)
    {
      if (strcmp (TREE_STRING_CST (name), 
                  TREE_STRING_CST (TREE_OPERAND (tel->element, 0))) == 0
          && ret == TREE_OPERAND (tel->element, 1)
          && type_lists_eq (TREE_OPERAND (tel->element, 2), args))
        return tel->element;
    }

  return NULL;
}

static inline tree
xmake_function_proto_args (int num, ...)
{
  va_list vl;
  tree ret;

  va_start (vl, num);
  ret = make_function_proto_args (num, vl);
  va_end (vl);

  return ret;
}


int
try_implicit_conversion (tree to_type, tree expr, tree *  container)
{
  int ret = 0;
  tree func_name;
  tree proto_args, proto;
    
  assert (expr != NULL && container != NULL, 0);
  
  if (TREE_TYPE (expr) == to_type)
    {
      *container = expr;
      return 0;
    }

  if (to_type == integer_type_node)
    func_name = make_string_cst_str ("int"); 
  else if (to_type == string_type_node)
    func_name = make_string_cst_str ("str"); 
  else if (to_type == list_type_node)
    func_name = make_string_cst_str ("list"); 
  else if (to_type ==  void_type_node)
    func_name = make_string_cst_str ("void"); 
  else
    unreachable ("Attempt to insert an implicit conversion to invalid type");
  
   /* Try to insert a conversion function.  */
   proto_args = xmake_function_proto_args (1, TREE_TYPE (expr));
   proto = match_proto_name_ret_args (func_name, to_type, proto_args);
   
   if (proto == NULL)
     {
       error_loc (TREE_LOCATION (expr), 
                  "Cannot convert the following expression to `%s':",
                  TREE_STRING_CST (func_name));
       print_expression (stderr, expr);
       fprintf (stderr, "\n");
       ret = 1;
     }
   else
     {
       /* Create a conversion function call.  */
       tree cexp = make_tree (CALL_EXPR);
       tree lst = make_tree_list ();

       TREE_OPERAND_SET (cexp, 0, TREE_OPERAND (proto, 0));
       tree_list_append (lst, expr);
       TREE_OPERAND_SET (cexp, 1, lst);
       TREE_TYPE (cexp) = to_type;
       *container = cexp;
       ret = 0;
     }

   free_list (proto_args);
   free_tree (func_name);
   return ret;
}

int zugma ()
{
  return 0;
}


int
typecheck_expression (tree expr, tree ext_vars, tree vars)
{
  int ret = 0;

  switch (TREE_CODE (expr))
    {
    case INTEGER_CST:
      TREE_TYPE (expr) = integer_type_node;
      TREE_CONSTANT (expr) = 1;
      break;
    case STRING_CST:
      TREE_TYPE (expr) = string_type_node;
      TREE_CONSTANT (expr) = true;
      break;
    case LIST_CST:
      {
        struct tree_list_element *  tel;

        TAILQ_FOREACH (tel, &TREE_LIST_QUEUE (TREE_LIST_CST (expr)), entries)
          {
            ret += typecheck_expression (tel->element, ext_vars, vars);
          }

        TREE_TYPE (expr) = list_type_node;
        TREE_CONSTANT (expr) = true;
      }
      break;
    case CALL_EXPR:
      {
        tree t = type_list_from_args (TREE_OPERAND (expr, 1), ext_vars, vars);
        tree proto;
        
        /*struct tree_list_element *  tel;
        TAILQ_FOREACH  (tel, &TREE_LIST_QUEUE (function_proto_list), entries)
          {
            struct tree_list_element * ptr;
            fprintf (stderr, "name = %s ", 
                     TREE_STRING_CST (TREE_OPERAND (tel->element,0)));
            fprintf (stderr, "ret = %s ", 
                     TREE_STRING_CST (TREE_TYPE_NAME (TREE_OPERAND (tel->element,1))));
            
            TAILQ_FOREACH (ptr, &TREE_LIST_QUEUE (TREE_OPERAND (tel->element, 2)), entries)
              {
                fprintf (stderr, "%s, ", TREE_STRING_CST (TREE_TYPE_NAME (ptr->element)));
              }
            fprintf (stderr, "\n");
          }
            fprintf (stderr, "\n");
            fprintf (stderr, "\n");*/

        if (t == NULL)
          {
            error_loc (TREE_LOCATION (expr),
                       "Type errors in the arguments disable the typecheking "
                       "of the function:");
            print_expression (stderr, expr);
            fprintf (stderr, "\n");
            return 1;
          }
        
        proto = match_proto_name_args (TREE_OPERAND (expr, 0), t);
        
        if (proto == NULL)
          {
            error_loc (TREE_LOCATION (expr), 
                       "Cannot find a function prototype for the following "
                       "function:");
            print_expression (stderr, expr);
            fprintf (stderr, "\n");
            ret = 1;
          }
        else
          {
            /* XXX may be we want to save the link to the prototype
               in the CALL_EXPR?  */
            TREE_TYPE (expr) = TREE_OPERAND (proto, 1);
          }
        free_list (t);
      }
      break;
    case IDENTIFIER:
      {
        tree var;
        
        /* The order of checking *is* important.  */
        if ((var = is_var_in_list (expr, vars)) != NULL
            || (var = is_var_in_list (expr, ext_vars)) != NULL
            || (var = is_var_in_consts (expr)) != NULL)
          {
            assert (TREE_TYPE (var) != NULL, 0);
            /* XXX save link to the VAR, because unfortunately
               we cannot replace it.  */
            COPY_TYPE_AND_ATTRS (expr, var);
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
    case COND_EXPR:
      {
        ret += typecheck_expression (TREE_OPERAND (expr, 0), ext_vars, vars);
        ret += typecheck_expression (TREE_OPERAND (expr, 1), ext_vars, vars);
        ret += typecheck_expression (TREE_OPERAND (expr, 2), ext_vars, vars);

        if (TREE_TYPE (TREE_OPERAND (expr, 0)) != integer_type_node)
          {
            error_loc (TREE_LOCATION (expr), 
                       "Condition of the conditional expression "
                       "must be integer");
            ret = 1;
          }

        if (TREE_TYPE (TREE_OPERAND (expr, 1)) 
            !=  TREE_TYPE (TREE_OPERAND (expr, 2)))
          {
            error_loc (TREE_LOCATION (expr), 
                       "Branches of the conditional expression "
                       "must be of the same type");
            ret = 1;
          }
        else
          TREE_TYPE (expr) = TREE_TYPE (TREE_OPERAND (expr, 1));
          /* Maybe we also want to deal with quals...  */
      }
      break;
    case MINUS_EXPR:
    case DIV_EXPR:
    case MULT_EXPR:
    case GT_EXPR:
    case LT_EXPR:
    case GE_EXPR:
    case LE_EXPR:
    case NE_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
      {
        tree lhs = TREE_OPERAND (expr, 0);
        tree rhs = TREE_OPERAND (expr, 1);
        int t;
        
        ret += typecheck_expression (lhs, ext_vars, vars);
        ret += typecheck_expression (rhs, ext_vars, vars);

        if (ret != 0 || TREE_TYPE (lhs) == NULL || TREE_TYPE (rhs) == NULL)
          return 1;
          
        if ((t = try_implicit_conversion (integer_type_node, lhs, &lhs)
            + try_implicit_conversion (integer_type_node, rhs, &rhs))
            == 0)
          {
            TREE_OPERAND_SET (expr, 0, lhs);
            TREE_OPERAND_SET (expr, 1, rhs);
            TREE_TYPE (expr) = integer_type_node;
          }
        else
          {
            return 1;
          }
      }
      break;
    case EQ_EXPR:
      {
        tree lhs = TREE_OPERAND (expr, 0);
        tree rhs = TREE_OPERAND (expr, 1);
        
        ret += typecheck_expression (lhs, ext_vars, vars);
        ret += typecheck_expression (rhs, ext_vars, vars);

        if (ret != 0 || TREE_TYPE (lhs) == NULL || TREE_TYPE (rhs) == NULL)
          return 1;

        /* XXX We set the result of the expression to int, however
           we do not check if we have an appropriate conversion
           function.  */
        if (TREE_TYPE (lhs) == TREE_TYPE (rhs))
          TREE_TYPE (expr) = integer_type_node;
        else
          {
            /* FIXME We can try to be smart here, but I am not really
               sure that we want it. For the time being produce an error.  */
            error_loc (TREE_LOCATION (expr),
                       "Comparing two incompatible types");
            return 1;
          }
      }
      break;
    case UMINUS_EXPR:
    case TRUTH_NOT_EXPR:
      {
        tree op = TREE_OPERAND (expr, 0);
        tree int_name = make_string_cst_str ("int");
        
        ret += typecheck_expression (op, ext_vars, vars);

        if (ret != 0 || TREE_TYPE (op) == NULL)
          {
            free_tree (int_name);
            return 1;
          }

        if (try_implicit_conversion (integer_type_node, op, &op) == 0)
          {
            TREE_OPERAND_SET (expr, 0, op);
            TREE_TYPE (expr) = integer_type_node;
          }
        else
          return 1;
      }
      break;
    
    case PLUS_EXPR:
      {
        tree lhs = TREE_OPERAND (expr, 0);
        tree rhs = TREE_OPERAND (expr, 1);

        ret += typecheck_expression (lhs, ext_vars, vars);
        ret += typecheck_expression (rhs, ext_vars, vars);

        if (ret != 0 || TREE_TYPE (lhs) == NULL || TREE_TYPE (rhs) == NULL)
          return 1;

        /* If any of elements of + is a sting, then all the 
           expression becomes string.  */
        if ((TREE_TYPE (lhs) == string_type_node 
             || TREE_TYPE (rhs) == string_type_node)
            && try_implicit_conversion (string_type_node, lhs, &lhs)
               + try_implicit_conversion (string_type_node, rhs, &rhs)
               == 0)
          {
            TREE_OPERAND_SET (expr, 0, lhs);
            TREE_OPERAND_SET (expr, 1, rhs);
            TREE_TYPE (expr) = string_type_node;
          }
        /* In case of lhs of + is list, we currently check only lhs
           of the expression, rhs could be anything, but not with the
           type NULL.  */
        else if ((TREE_TYPE (lhs) == list_type_node 
                  || TREE_TYPE (rhs) == list_type_node)
                 && try_implicit_conversion (list_type_node, lhs, &lhs)
                    + typecheck_expression (rhs, ext_vars, vars)
                    == 0)
          {
            TREE_OPERAND_SET (expr, 0, lhs);
            TREE_OPERAND_SET (expr, 1, rhs);
            TREE_TYPE (expr) = list_type_node;
          }
        else if ((TREE_TYPE (lhs) == integer_type_node 
                  || TREE_TYPE (rhs) == integer_type_node)
                 && try_implicit_conversion (integer_type_node, lhs, &lhs)
                    + try_implicit_conversion (integer_type_node, rhs, &rhs)
                    == 0)
          {
            TREE_OPERAND_SET (expr, 0, lhs);
            TREE_OPERAND_SET (expr, 1, rhs);
            TREE_TYPE (expr) = integer_type_node;
          }
        else
          {
            error_loc (TREE_LOCATION (expr),
                       "Arguments of + operation are neither integers, "
                       " strings or list");
            return 1;
          }
      }
      break;

    case MOD_EXPR:
      {
        tree lhs = TREE_OPERAND (expr, 0);
        tree rhs = TREE_OPERAND (expr, 1);

        ret += typecheck_expression (lhs, ext_vars, vars);
        ret += typecheck_expression (rhs, ext_vars, vars);

        if (ret != 0 || TREE_TYPE (lhs) == NULL || TREE_TYPE (rhs) == NULL)
          return 1;

        /* If the second argument of % is list or the first argument
           is a sting, then we check for formatting operation.  */
        if ((TREE_TYPE (lhs) == string_type_node 
             || TREE_TYPE (rhs) == list_type_node)
            && try_implicit_conversion (string_type_node, lhs, &lhs)
               + try_implicit_conversion (list_type_node, rhs, &rhs)
               == 0)
          {
            struct tree_list_element *  tel;

            TREE_OPERAND_SET (expr, 0, lhs);
            TREE_OPERAND_SET (expr, 1, rhs);

            if (TREE_CODE (rhs) != LIST_CST)
              {
                error_loc (TREE_LOCATION (lhs),
                           "Format arguments must be stored "
                           "in a constant list");
                return 1;
              }

            TAILQ_FOREACH (tel, &TREE_LIST_QUEUE (TREE_LIST_CST (rhs)), entries)
              {
                tree el = tel->element;
                ret += try_implicit_conversion (string_type_node, el, &el);
                if (ret == 0)
                  tel->element = el;
                else
                  return 1;
              }

            TREE_TYPE (expr) = string_type_node;
            TREE_CONSTANT (expr) = TREE_CONSTANT (lhs);
          }
        /* two integers means modulo.  */
        else if ((TREE_TYPE (lhs) == integer_type_node 
                  || TREE_TYPE (rhs) == integer_type_node)
                 && try_implicit_conversion (integer_type_node, lhs, &lhs)
                    + try_implicit_conversion (integer_type_node, rhs, &rhs)
                    == 0)
          {
            TREE_OPERAND_SET (expr, 0, lhs);
            TREE_OPERAND_SET (expr, 1, rhs);
            TREE_TYPE (expr) = integer_type_node;
          }
        else
          {
            error_loc (TREE_LOCATION (expr),   
                       "Arguments of %% operation are neither integers or "
                       " string and list");
            return 1;
          }
      }
      break;

    default:
      error ("cannot typecheck expression of type `%s'",
             TREE_CODE_NAME (TREE_CODE (expr)));
      unreachable (0);
    }

  return ret;
}


/*int
typecheck_statement (tree stmt)
{
  return 0;
}*/



