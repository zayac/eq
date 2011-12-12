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
#include "codegen.h"

#define indent(f, x)  \
  do { \
    int __i;  \
    for (__i = 0; __i < (x); __i++) \
      fprintf (f, "\t");  \
  } while (0)

static int level;
int 
codegen ()
{
  struct tree_list_element *tl;
  int function_error = 0;
  const char* filename = "out.py";
  FILE* f;
  level = 0;

  /* File to write files to.  */
  if ((f = fopen (filename, "w")) == NULL)
    {
      fprintf (stderr, "Can't open file `%s' for writing",
	       filename);
      return 1;
    }

  DL_FOREACH (TREE_LIST (function_list), tl)
    function_error += codegen_function (f, tl->entry);

  fprintf (f, "\nif __name__ == '__main__':\n\t__main()\n");
  fclose (f);
  printf ("note: finished generating code.\n");
  return function_error;
}

int 
codegen_function (FILE* f, tree func)
{
  struct tree_list_element *el;
  int error = 0;
  char* func_name = TREE_STRING_CST (TREE_ID_NAME (TREE_OPERAND (func, 0))); 

  assert (TREE_CODE (func) == FUNCTION, "function tree expected");
  if (strcmp(func_name, "\\mu"))
    fprintf(f, "def %s(", func_name);
  else
    fprintf(f, "def __main(");
  DL_FOREACH (TREE_LIST (TREE_OPERAND (func, 1)), el)
    {
      fprintf (f, "%s",
	       TREE_STRING_CST (TREE_ID_NAME (el->entry)));
      if (el->next != NULL)
	fprintf (f, ", ");
    }
  fprintf (f, "):\n");
  level++;
  error = codegen_stmt_list (f, TREE_OPERAND (func, 4), func_name); 
  level--;
  indent (f, level);
  return error;
}

int
codegen_stmt_list (FILE* f, tree stmt_list, char* func_name)
{
  struct tree_list_element *tle;
  int error = 0;
  assert (TREE_CODE (stmt_list) == LIST, "statement list expected");
  DL_FOREACH (TREE_LIST (stmt_list), tle)
    error += codegen_stmt (f, tle->entry, func_name);
  return error;
}

int
codegen_stmt (FILE* f, tree stmt, char* func_name)
{
  int error = 0;
  indent (f, level); 
  switch (TREE_CODE (stmt))
    {
    case ASSIGN_STMT:
      {
	codegen_expression (f, TREE_OPERAND (stmt, 0));
    	fprintf (f, " = ");
	codegen_expression (f, TREE_OPERAND (stmt, 1));
      }
      break;
    case DECLARE_STMT:
      {
	tree type = TREE_OPERAND (stmt, 1);
	codegen_expression (f, TREE_OPERAND (stmt, 0));
	fprintf (f, " = ");
	if (type == z_type_node || type == n_type_node)
	  fprintf (f, "0");
	else if (type == r_type_node)
	  fprintf (f, "0.0");
	else
	  {
	    if (TYPE_SHAPE (type) != NULL)
	      {
		struct tree_list_element *el;
		unsigned counter = 0;
		fprintf (f, "(");
		DL_FOREACH (TREE_LIST (TYPE_SHAPE (type)), el)
		  {
		    error += codegen_expression (f, el->entry);
		    if (el->next == NULL)
		      fprintf (f, "*[0]");
		    else
		      fprintf (f, "*[");
		    counter++;
		  }
		while (--counter)
		  fprintf(f, "]");
		fprintf (f, ")");
	      }
	    else if (TYPE_DIM (type) != NULL)
	      {
		assert (TREE_CODE (TYPE_DIM (type)) == INTEGER_CST,
			"only integer dimension is supported by now.");
		if (TREE_CODE (TYPE_DIM (type)) == INTEGER_CST)
		  {
		    unsigned dim = TREE_INTEGER_CST (TYPE_DIM (type));
		    int i;
		    for (i = 0; i < dim * 2; i++)
		      if (i < dim)
			fprintf (f, "[");
		      else  
			fprintf (f, "]");
		  }
	      }
	  }
      }
      break;
    case WITH_LOOP_EXPR:
      {
	tree id;
	struct tree_list_element *el;
	if (TREE_CODE (TREE_OPERAND (stmt, 0)) == LOWER)
	  id = TREE_OPERAND (TREE_OPERAND (stmt, 0), 0);
	else
	  id = TREE_OPERAND (TREE_OPERAND (TREE_OPERAND (stmt, 0), 0), 0);
	assert (TREE_CODE (id) == IDENTIFIER, "identifier expected");
	/* FIXME We don't distinguish a newly defined list and an existing one.
	   In this case, if an array was defined already, and we want to change
	    *some*(not all) values in it, we are to include old values into
	    cases as well.  */
	error += codegen_expression (f, id);
	fprintf (f, " = ");
	//if (TYPE_SHAPE (TREE_TYPE (id)) == NULL)
	  {
	    int lowerb = 0, upperb = 0;
	    tree it = NULL;
	    int case_counter = 0;
	    tree bound = TREE_OPERAND (TREE_OPERAND (stmt, 1), 1);
	    /* FIXME only generators '<const> < <variable> < <const>' are
	       supported at the moment.  */
	    assert (TREE_CODE (bound) == LAND_EXPR
	         && TREE_CODE (TREE_OPERAND (bound, 0)) == LE_EXPR
		 && TREE_CODE (TREE_OPERAND (bound, 1)) == LE_EXPR
		 && TREE_CODE (TREE_OPERAND (TREE_OPERAND (bound, 0), 1))
		      == IDENTIFIER
		 && TREE_CODE (TREE_OPERAND (TREE_OPERAND (bound, 1), 0))
		      == IDENTIFIER, "only generators '<const < <variable> < "
		      "<const>' are supported at the moment");

	    lowerb = TREE_INTEGER_CST (TREE_OPERAND (
				       TREE_OPERAND (bound, 0), 0));
	    upperb = TREE_INTEGER_CST (TREE_OPERAND (
				       TREE_OPERAND (bound, 1), 1));
	    it = TREE_OPERAND (TREE_OPERAND (bound, 1), 0);
	    fprintf (f, "[");
	    DL_FOREACH (TREE_LIST (TREE_OPERAND (stmt, 2)), el)
	      {
		fprintf (f, " ");
		error += codegen_expression (f, TREE_OPERAND (el->entry, 0));
		if (el->next != NULL)
		  {
		    fprintf (f, " if ");
		    error += codegen_expression (f, 
						 TREE_OPERAND (el->entry, 1));
		    fprintf (f, " else");
		  }
		case_counter++;
	      }
	    fprintf (f, " for %s in range(%d,%d)]",
			TREE_STRING_CST (TREE_ID_NAME (it)),
			lowerb,
			upperb);
	  }
      }
      break;
    case RETURN_STMT:
      {
	fprintf (f, "print('%s -> {}'.format(", func_name);
	error += codegen_expression (f, TREE_OPERAND (stmt, 0));
	fprintf (f, "))\n");
	indent(f, level);
	fprintf (f, "return(");
	codegen_expression (f, TREE_OPERAND (stmt, 0));
	fprintf (f, ")");
      }
      break;
    case IF_STMT:
      {
	fprintf (f, "if ");
	error += codegen_expression (f, TREE_OPERAND (stmt, 0));
	fprintf (f, ":\n");
	level++;
	error += codegen_stmt_list (f, TREE_OPERAND (stmt, 1), func_name);
	level--;
	indent (f, level);
	if (TREE_OPERAND (stmt, 2) != NULL)
	  {
	    fprintf (f, "else:\n");
	    level++;
	    error += codegen_stmt_list (f, TREE_OPERAND (stmt, 2), func_name);
	    level--;
	  }
      }
      break;
    default:
      {
	error_loc (TREE_LOCATION (stmt), "cannot generate code for "
		   "statement of type `%s'",
		   TREE_CODE_NAME (TREE_CODE (stmt)));
	error += 1;
      }
      return error;
    }
    fprintf (f, "\n");
  return error;
}

int 
codegen_expression (FILE* f, tree expr)
{
  int error = 0;

  switch (TREE_CODE (expr))
    {
    case INTEGER_CST:
      {
	if (TREE_TYPE (expr) == z_type_node)
	  fprintf (f, "%d", TREE_INTEGER_CST (expr));
	else if (TREE_TYPE (expr) == n_type_node)
	  fprintf (f, "%u", TREE_INTEGER_CST (expr));
	else
	  {
	    error_loc (TREE_LOCATION (expr), "invalid integer type `%s'",
		       TREE_CODE_NAME (TREE_CODE (TREE_TYPE (expr))));
	    fprintf(f, "<ERROR>");
	    error += 1;
	  }
      }
      break;
    case REAL_CST:
      fprintf (f, "%f", TREE_REAL_CST (expr));
      break;
    case STRING_CST:
      fprintf (f, "\"");
      if ((TREE_STRING_CST (expr))[0] == '\\')
	{
	  char* c = TREE_STRING_CST (expr);
	  fprintf (f, "_");
	  while (*(++c)) 
	    {
	      fprintf (f, "%c", *c);
	    }
	}
      else
	fprintf (f, "%s", TREE_STRING_CST (expr));
      fprintf (f, "\"");
      break;
    case IDENTIFIER:
      {
	char* c = TREE_STRING_CST (TREE_ID_NAME (expr));
	if (*c == '\\')
	  {
	    fprintf (f, "_");
	    while (*(++c)) 
	      {
		fprintf (f, "%c", *c);
	      }
	  }
	else
	  fprintf (f, "%s", c);
      }
      break;
    case FUNCTION_CALL:
      {
	struct tree_list_element *el;
	fprintf(f, "%s(",
		TREE_STRING_CST (TREE_ID_NAME (TREE_OPERAND (expr, 0))));
	DL_FOREACH (TREE_LIST (TREE_OPERAND (expr, 1)), el)
	  {
	    fprintf (f, "%s", TREE_STRING_CST (TREE_ID_NAME (el->entry)));
	    if (el->next != NULL)
	      fprintf (f, ", ");
	  }
	fprintf (f, ")\n");
      }
      break;
    case MATRIX_EXPR:
      {	
	struct tree_list_element *eli, *elj;
	fprintf (f, "[");
	DL_FOREACH (TREE_LIST (TREE_OPERAND (expr, 0)), eli)
	  {
	    bool type_dim;
	    assert (TREE_CODE (TYPE_DIM (TREE_TYPE (expr)))  == INTEGER_CST, 
		    "only integer dimensions are supported in the types for "
		    "matrixes");
	    type_dim = TREE_INTEGER_CST (TYPE_DIM (TREE_TYPE (expr)));
	    if (type_dim != 1)
	      fprintf (f, "[");
	    DL_FOREACH (TREE_LIST (eli->entry), elj)
	      {
		error += codegen_expression (f, elj->entry);
		if (elj->next != NULL)
		  fprintf (f, ", ");
	      }
	    if (type_dim != 1)
	      fprintf (f, "]");
	    if (eli->next != NULL)
	      fprintf (f, ", ");
	  }
	fprintf (f, "]");
      }
      break;
    case LOWER:
      {
	struct tree_list_element *el;
	error += codegen_expression (f, TREE_OPERAND (expr, 0));
	fprintf (f, "[");
	DL_FOREACH (TREE_LIST (TREE_OPERAND (expr, 1)), el)
	  {
	    error += codegen_expression (f, el->entry);
	    if (el->next != NULL)
	      fprintf (f, ", ");
	  }
	fprintf (f, "]");
      }
      break;
    case GENERATOR:
      {
	error += codegen_expression (f, TREE_OPERAND (expr, 1));
      }
      break;
    case CIRCUMFLEX:
      {
	assert (!TREE_CIRCUMFLEX_INDEX_STATUS (expr), "circumflex as index "
		"is not supported by now");
	fprintf (f, "(");
	error += codegen_expression (f, TREE_OPERAND (expr, 0));
	fprintf (f, "**");
	error += codegen_expression (f, TREE_OPERAND (expr, 1));
	fprintf (f, ")");
      }
      break;
    case CONVERT_EXPR:
      {
	error += codegen_expression (f, TREE_OPERAND (expr, 0));
      }
      break;
    case UMINUS_EXPR:
      {
	fprintf (f, "-");
	error += codegen_expression (f, TREE_OPERAND (expr, 0));
      }
      break;
    case NOT_EXPR:
      {
	fprintf (f, "~");
	error += codegen_expression (f, TREE_OPERAND (expr, 0));
      }
    default:
      {
	const char* opcode;
	switch (TREE_CODE (expr))
	  {
	    case EQ_EXPR:
	      opcode = "==";
	      break;
	    case GT_EXPR:
	      opcode = ">";
	      break;
	    case LT_EXPR:
	      opcode = "<";
	      break;
	    case GE_EXPR:
	      opcode = ">=";
	      break;
	    case LE_EXPR:
	      opcode = "<=";
	      break;
	    case NE_EXPR:
	      opcode = "!=";
	      break;
	    case LOR_EXPR:
	      opcode = "or";
	      break;
	    case LAND_EXPR:
	      opcode = "and";
	      break;
	    case BOR_EXPR:
	      opcode = "|";
	      break;
	    case BAND_EXPR:
	      opcode = "&";
	      break;
	    case XOR_EXPR:
	      opcode = "^";
	      break;
	    case SRIGHT_EXPR:
	      opcode = "<<";
	      break;
	    case SLEFT_EXPR:
	      opcode = ">>";
	      break;
	    case PLUS_EXPR:
	      opcode = "+";
	      break;
	    case MINUS_EXPR:
	      opcode = "-";
	      break;
	    case DIV_EXPR:
	      opcode = "/";
	      break;
	    case MULT_EXPR:
	      opcode = "*";
	      break;
	    case MOD_EXPR:
	      opcode = "%";
	      break;
	    default:
	      {
		error_loc (TREE_LOCATION (expr), "cannot generate code for "
			   "expression of type `%s'",
			   TREE_CODE_NAME (TREE_CODE (expr)));
		error += 1;
	      }
	    return error;
	  }
	fprintf (f, "(");
	error += codegen_expression (f, TREE_OPERAND (expr, 0));
	fprintf (f, " %s ", opcode);
	error += codegen_expression (f, TREE_OPERAND (expr, 1));
	fprintf (f, ")");
      }
    }
  return error;
}
