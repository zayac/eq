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
#include "codegen.h"

#define indent(f, x)			      \
  do {					      \
    int __i;				      \
    for (__i = 0; __i < (x); __i++)	      \
      fprintf (f, "\t");		      \
  } while (0)


static int level;
static int codegen_genar_function (FILE*);

extern tree iter_var_list;

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
      fprintf (stderr, "Can't open file `%s' for writing", filename);
      return 1;
    }

  fprintf (f, "import sys\n");
  fprintf (f, "import itertools\n\n");

  codegen_genar_function (f);

  DL_FOREACH (TREE_LIST (iter_var_list), tl)
    {
      level = 0;
      codegen_iterative (f, tl->entry);
    }

  DL_FOREACH (TREE_LIST (function_list), tl)
    {
      level = 0;
      function_error += codegen_function (f, tl->entry);
    }
  fprintf (f, "\nif sys.version_info < (3, 0):\n\trange = xrange\n");
  fprintf (f, "\nif __name__ == '__main__':\n\t__main()\n");
  fclose (f);
  printf ("note: finished generating code.\n");

  return function_error;
}

static int
codegen_genar_function (FILE* f)
{
  indent(f, 0);
  fprintf (f, "def __genarray (shape, value):\n"
	      "\tret, tmp =[], []\n"
	      "\tfor i in range(len(shape)-1,-1,-1):\n"
	      "\t\tret=list(tmp)\n"
	      "\t\ttmp=[]\n"
	      "\t\tfor j in range(shape[i]):\n"
	      "\t\t\tif i == len(shape)-1:\n"
	      "\t\t\t\ttmp.append(value)\n"
	      "\t\t\telse:\n"
	      "\t\t\t\ttmp.append(list(ret))\n"
	      "\treturn (tmp)\n");
  return 0;
}

int
codegen_function (FILE* f, tree func)
{
  struct tree_list_element *  el;
  int error = 0;
  char *  func_name = TREE_STRING_CST (TREE_ID_NAME (TREE_OPERAND (func, 0)));


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
    {
      level = 1;
      /* We skip declare statements.  */
      if (TREE_CODE (tle->entry) != DECLARE_STMT)
	error += codegen_stmt (f, tle->entry, func_name);
    }
  return error;
}

/* FIXME Local variables can't be used in the assignment at the moment. This is
   due to iterative expression is calculated in separate function and local
   variables aren't passed to this function. */
int
codegen_iterative (FILE* f, tree var)
{
  int error = 0;
  struct tree_list_element *el; 
  fprintf (f, "def __recur_%s(i):\n", TREE_STRING_CST (TREE_ID_NAME (var)));
  fprintf (f, "\tif i in __recur_%s.value:\n",
    TREE_STRING_CST (TREE_ID_NAME (var)));
  fprintf (f, "\t\treturn (__recur_%s.value[i])\n", 
    TREE_STRING_CST (TREE_ID_NAME (var)));
  DL_FOREACH (TREE_LIST (TREE_ID_ITER (var)), el)
    {
      fprintf (f, "\t__recur_%s.value[", TREE_STRING_CST (TREE_ID_NAME (var)));
      error += codegen_expression (f, TREE_OPERAND (el->entry, 0));
      fprintf (f, "]=");
      error += codegen_expression (f, TREE_OPERAND (el->entry, 1));
      fprintf (f, "\n");
      if (TREE_CODE (TREE_OPERAND (el->entry, 0)) == INTEGER_CST)
	{
	  fprintf (f, "\tif i == ");
	  error += codegen_expression (f, TREE_OPERAND (el->entry, 0));
	  fprintf (f, ":\n");
	  fprintf (f, "\t\treturn (__recur_%s.value[i])\n", 
	    TREE_STRING_CST (TREE_ID_NAME (var)));
	} 
    }
  fprintf (f, "\treturn (__recur_%s.value[i])\n", 
    TREE_STRING_CST (TREE_ID_NAME (var)));
  fprintf (f, "__recur_%s.value={}\n", TREE_STRING_CST (TREE_ID_NAME (var)));
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
	if (TREE_CODE (TREE_OPERAND (stmt, 0)) == CIRCUMFLEX
	    && TREE_CIRCUMFLEX_INDEX_STATUS (stmt))
	  {
	    tree id;
	    if (TREE_CODE (TREE_OPERAND (TREE_OPERAND (stmt, 0), 0))
	      == IDENTIFIER)
	      id = TREE_OPERAND (TREE_OPERAND (stmt, 0), 0);
	    else
	      id = TREE_OPERAND (TREE_OPERAND (TREE_OPERAND (stmt, 0), 0), 0);
	    //codegen_iterative (f, id);
	  }
	else
	  {
	    codegen_expression (f, TREE_OPERAND (stmt, 0));
	    fprintf (f, " = ");
	    codegen_expression (f, TREE_OPERAND (stmt, 1));
	  }
      }
      break;
    case DECLARE_STMT:
      {
	/* We skip declare (`\in') statement in python.  */  
      }
      return error;
    case INDEX_LOOP_EXPR:
      {
	tree id, gen_id_list;
	unsigned counter = 0;
	struct tree_list_element *el, * tel;

	if (TREE_CODE (TREE_OPERAND (stmt, 0)) == LOWER)
	  id = TREE_OPERAND (TREE_OPERAND (stmt, 0), 0);
	else
	  id = TREE_OPERAND (TREE_OPERAND (TREE_OPERAND (stmt, 0), 0), 0);

	assert (TREE_CODE (id) == IDENTIFIER, "identifier expected");
	gen_id_list = TREE_OPERAND (TREE_OPERAND (stmt, 1), 0);
	counter = 0;

	/* iterate through all indexes.  */
	fprintf (f, "for (");
    	DL_FOREACH (TREE_LIST (gen_id_list), el)
	  {
	    error += codegen_expression (f, el->entry);
	    if (el->next != NULL)
	      fprintf (f, ", ");
	    counter++;
	  }
	fprintf (f, ") in ");
	if (counter > 1)
	  fprintf (f, "itertools.product(");
	counter = 0;
	DL_FOREACH (TREE_LIST (gen_id_list), el)
	  {
	    int i = 0;
	    fprintf (f, "range(len(%s",
		  TREE_STRING_CST (TREE_ID_NAME (id)));
	    DL_FOREACH (TREE_LIST (gen_id_list), tel)
	      {
		if (i++ == counter)
		  break;
		fprintf (f, "[%d]", counter-1);
	      }
	    counter++;
	    fprintf (f, "))");
	    if (el->next != NULL)
	      fprintf (f, ", ");
	  }
	if (counter > 1)
	  fprintf (f, ")");
	fprintf (f, ":\n");
	level++;
	indent (f, level);
	/* replace value only if condition is satisfied.  */
	if (TREE_CODE (TREE_OPERAND (stmt, 1)) == GENERATOR)
	  {
	    fprintf (f, "if ");
	    error += codegen_expression (f, 
		TREE_OPERAND (TREE_OPERAND (stmt, 1), 1));
	    fprintf (f, ":\n");
	    level++;
	    indent (f, level);
	  }
	counter = 0;
	DL_FOREACH (TREE_LIST (TREE_OPERAND (stmt, 2)), el)
	  {
	    if (el->next == NULL)
	      {
		if (counter)
		  {
		    fprintf (f, "else:\n");
		    level++;
		    indent (f, level);
		  }
	      }
	    else
	      {
		if (el == TREE_LIST (TREE_OPERAND (stmt, 2)))
		  fprintf (f, "if");
		else
		  fprintf (f, "elif");
		error += codegen_expression (f, TREE_OPERAND (el->entry, 1));
		fprintf (f, ":\n");
		level++;
		indent (f, level);
	      }
	    error += codegen_expression (f, id);
	    DL_FOREACH (TREE_LIST (gen_id_list), tel)
	      {
		fprintf (f, "[");
		error += codegen_expression (f, tel->entry);
		fprintf (f, "]");
	      }
	    fprintf (f, " = ");
	    error += codegen_expression (f, TREE_OPERAND (el->entry, 0));
	    fprintf (f, "\n");
	    level--;
	    counter++;
	    if (el->next != NULL)
	      indent (f, level);
	  }
	if (TREE_CODE (TREE_OPERAND (stmt, 1)) == GENERATOR)
	  {
	    level -= 2;
	    if (TREE_LIST (TREE_OPERAND (stmt, 2))->next == NULL)
	      level++;
	    indent (f, level);
	  }
      }
      break;

    case RETURN_STMT:
      {
	/* print return value of main `\mu' function.  */
	if (!strcmp(func_name, "\\mu"))
	  {
	    fprintf (f, "print(");
	    error += codegen_expression (f, TREE_OPERAND (stmt, 0));
	    fprintf (f, ")\n");
	  }
	indent (f, level);
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
	return error;
      }
    }

  fprintf (f, "\n");
  return error;
}

int
codegen_genar (FILE* f, tree expr, struct tree_list_element *shape)
{
  int error = 0;

  if (shape == NULL)
    return codegen_expression (f, expr);

  fprintf (f, "[");
  error += codegen_genar (f, expr, shape->next);

  /* FIXME: Make the internal variables of different names.  */
  fprintf (f, " for __i in range (");

  error += codegen_expression (f, TREE_LIST (shape->entry)->entry);
  fprintf (f, ")]");
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

    /* FIXME: This is wrong -- python strings support escape-characters.
       Also, why only first symbol of the string is being checked?  */
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
	    codegen_expression (f, el->entry);
	    if (el->next != NULL)
	      fprintf (f, ", ");
	  }
	fprintf (f, ")");
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
	      fprintf (f, "][");
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
	//assert (!TREE_CIRCUMFLEX_INDEX_STATUS (expr), "circumflex as index "
	//	"is not supported by now");

	if (!TREE_CIRCUMFLEX_INDEX_STATUS (expr))
	  {
	    fprintf (f, "(");
	    error += codegen_expression (f, TREE_OPERAND (expr, 0));
	    fprintf (f, "**");
	    error += codegen_expression (f, TREE_OPERAND (expr, 1));
	    fprintf (f, ")");
	  }
	else
	  {
	    fprintf (f, "__recur_%s(", 
	      TREE_STRING_CST (TREE_ID_NAME (TREE_OPERAND (expr, 0))));
	    error += codegen_expression (f, TREE_OPERAND (expr, 1));
	    fprintf (f, ")");
	  }
      }
      break;

    /* FIXME Check if an explicit conversion is needed.  */
    case CONVERT_EXPR:
      {
	error += codegen_expression (f, TREE_OPERAND (expr, 0));
      }
      break;

    case GENAR_EXPR:
      {
	if (TREE_TYPE (TREE_OPERAND (expr, 0)) == z_type_node)
	  {
	    fprintf (f, "[");
	    error += codegen_expression (f, TREE_OPERAND (expr, 1));
	    fprintf (f, " for __i in range(");
	    error += codegen_expression (f, TREE_OPERAND (expr, 0));
	    fprintf (f, ")]");
	  }
	else if (TREE_CODE (TREE_OPERAND (expr, 0)) == MATRIX_EXPR)
	  error += codegen_genar (f, TREE_OPERAND (expr, 1),
				  TREE_LIST (TREE_OPERAND (
				  TREE_OPERAND (expr, 0), 0)));
	else
	  {
	    /* it should be variable of some vector type.  */
	    assert (TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == Z_TYPE
		 && TREE_INTEGER_CST (
		    TYPE_DIM (TREE_TYPE (TREE_OPERAND (expr, 0)))) == 1,
		 "unexpected type");
	    fprintf (f, "__genarray(");
	    error += codegen_expression (f, TREE_OPERAND (expr, 0));
	    fprintf (f, ", ");
	    error += codegen_expression (f, TREE_OPERAND (expr, 1));
	    fprintf (f, ")");
	  }
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
      break;

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
