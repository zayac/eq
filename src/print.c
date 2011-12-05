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

#include "expand.h"
#include "tree.h"
#include "print.h"
#include "global.h"

static int level = 0;

#define indent(f, x)   \
    do { \
        int __i; \
        for (__i = 0; __i < (x); __i++) \
          fprintf (f, "  "); \
    } while (0)

int
print_expression (FILE * f, tree exp)
{
  /*if (exp != NULL)
     printf ("-- enter function %s with exp %s\n", __func__, 
     TREE_CODE_NAME (TREE_CODE (exp))); */
  //printf("%s\n", TREE_CODE_NAME(TREE_CODE(exp)));
  assert (exp != NULL
	  && (TREE_CODE (exp) == FUNCTION || TREE_CODE (exp) == LIST

	      || TREE_CODE_CLASS (TREE_CODE (exp)) == tcl_type
	      || TREE_CODE_CLASS (TREE_CODE (exp)) == tcl_expression
	      || TREE_CODE_CLASS (TREE_CODE (exp)) == tcl_constant
	      || TREE_CODE (exp) == IDENTIFIER
	      || TREE_CODE (exp) == ERROR_MARK
	      || TREE_CODE (exp) == IF_STMT
	      || TREE_CODE (exp) == DECLARE_STMT
	      || TREE_CODE (exp) == ASSIGN_STMT
	      || TREE_CODE (exp) == RETURN_STMT),
	  "attempt to print non-expression tree %s",
	  TREE_CODE_NAME (TREE_CODE (exp)));

  if (options.print_types)
    fprintf (f, "{");

  switch (TREE_CODE (exp))
    {
    case ERROR_MARK:
      fprintf (f, "<<ERROR>>");
      return 0;
    case STRING_CST:
      fprintf (f, "%s", TREE_STRING_CST (exp));
      break;
    case REAL_CST:
      fprintf (f, "%f", TREE_REAL_CST (exp));
      break;
    case INTEGER_CST:
      fprintf (f, "%i", TREE_INTEGER_CST (exp));
      break;
    case LIST:
      {
	struct tree_list_element *tle = NULL;
	DL_FOREACH (TREE_LIST (exp), tle)
	{
	  print_expression (f, tle->entry);
	  if (tle->next != NULL)
	    fprintf (f, ", ");
	}
	break;
      }
    case IDENTIFIER:
      print_expression (f, TREE_ID_NAME (exp));
      break;
    case FUNCTION_CALL:
      {
	fprintf (f, "\\call{");
	print_expression (f, TREE_OPERAND (exp, 0));
	fprintf (f, "}{");
	if (TREE_OPERAND (exp, 1) != NULL)
	  print_expression (f, TREE_OPERAND (exp, 1));
	fprintf (f, "}");
	break;
      }
    case DIV_EXPR:
      {
	fprintf (f, "\\frac{");
	print_expression (f, TREE_OPERAND (exp, 0));
	fprintf (f, "}{");
	print_expression (f, TREE_OPERAND (exp, 1));
	fprintf (f, "}");
	break;
      }
    case GENAR_EXPR:
      {
	fprintf (f, "\\genar \\limits ^ { ");
	print_expression (f, TREE_OPERAND (exp, 0));
	fprintf (f, " } ( ");
	print_expression (f, TREE_OPERAND (exp, 1));
	fprintf (f, " ) ");
	break;
      }
    case CIRCUMFLEX:
      {
	print_expression (f, TREE_OPERAND (exp, 0));
	fprintf (f, "^{");
	if (TREE_CIRCUMFLEX_INDEX_STATUS (exp))
	  fprintf (f, "[");
	print_expression (f, TREE_OPERAND (exp, 1));
	if (TREE_CIRCUMFLEX_INDEX_STATUS (exp))
	  fprintf (f, "]");
	fprintf (f, "}");
	break;
      }
    case FUNCTION:
      {
	struct tree_list_element *tle = NULL;
	assert (TREE_CODE (TREE_FUNC_INSTRS (exp)) == LIST, 0);
	fprintf (f, "\\begin{ eqcode }{");
	print_expression (f, TREE_FUNC_NAME (exp));
	fprintf (f, "}{");
	if (TREE_FUNC_ARGS (exp) != NULL)
	  print_expression (f, TREE_FUNC_ARGS (exp));
	fprintf (f, "}{");
	if (TREE_FUNC_ARGS_TYPES (exp) != NULL)
	  print_expression (f, TREE_FUNC_ARGS_TYPES (exp));
	fprintf (f, "}{");
	print_expression (f, TREE_FUNC_RET_TYPE (exp));
	fprintf (f, "}\n");
	level += 2;
	DL_FOREACH (TREE_LIST (TREE_FUNC_INSTRS (exp)), tle)
	{
	  indent (f, level);
	  print_expression (f, tle->entry);
	  fprintf (f, " \\lend\n");
	}
	level -= 2;
	fprintf (f, "\\end{eqcode}\n");
	break;
      }
    case IF_STMT:
      {
	fprintf (f, "\\qif { ");
	print_expression (f, TREE_OPERAND (exp, 0));
	fprintf (f, " }\n");
	level += 2;
	indent (f, level);
	print_expression (f, TREE_OPERAND (exp, 1));
	fprintf (f, "\n");
	level -= 2;
	indent (f, level);
	if (TREE_OPERAND (exp, 2) != NULL)
	  {
	    fprintf (f, "\\else\n");
	    level += 2;
	    indent (f, level);
	    print_expression (f, TREE_OPERAND (exp, 2));
	    level -= 2;
	    indent (f, level);
	  }
	fprintf (f, "\\qendif\n");
	break;
      }
    case B_TYPE:
    case N_TYPE:
    case Z_TYPE:
    case R_TYPE:
      {
	fprintf (f, "\\type {");
	if (TREE_CODE (exp) == B_TYPE)
	  fprintf (f, "B");
	else if (TREE_CODE (exp) == N_TYPE)
	  fprintf (f, "N");
	else if (TREE_CODE (exp) == Z_TYPE)
	  fprintf (f, "Z");
	else
	  fprintf (f, "R");

	if (TYPE_DIM (exp) != NULL)
	  {
	    fprintf (f, "^{");
	    print_expression (f, TYPE_DIM (exp));
	    fprintf (f, "}");
	    if (TYPE_SHAPE (exp) != NULL)
	      {
		fprintf (f, "_{");
		print_expression (f, TYPE_SHAPE (exp));
	      }
	    else
	      return fprintf (f, "}");
	  }
	fprintf (f, "}");
	break;
      }
    case FILTER_EXPR:
      {
	fprintf (f, "\\filter { ");
	print_expression (f, TREE_OPERAND (exp, 0));
	fprintf (f, " | ");
	print_expression (f, TREE_OPERAND (exp, 1));
	fprintf (f, " } ");
	break;
      }
    case UMINUS_EXPR:
      {
	fprintf (f, " -");
	print_expression (f, TREE_OPERAND (exp, 0));
	break;
      }
    case NOT_EXPR:
      {
	fprintf (f, "\\lnot ");
	print_expression (f, TREE_OPERAND (exp, 0));
	break;
      }
    case RETURN_STMT:
      {
	fprintf (f, "\\return {");
	print_expression (f, TREE_OPERAND (exp, 0));
	fprintf (f, "} ");
	break;
      }
    case FORALL:
      {
	fprintf (f, "\\forall ");
	print_expression (f, TREE_OPERAND (exp, 0));
	break;
      }
    case WITH_LOOP_EXPR:
      {
	struct tree_list_element *tle = NULL;
	print_expression (f, TREE_OPERAND (exp, 0));
	fprintf (f, " | ");
	print_expression (f, TREE_OPERAND (exp, 1));
	fprintf (f, " = ");
	fprintf (f, "\\begin{cases}\n");
	level += 2;
	DL_FOREACH (TREE_LIST (TREE_OPERAND (exp, 2)), tle)
	{
	  indent (f, level);
	  print_expression (f, tle->entry);
	  if (tle->next != NULL)
	    fprintf (f, " \\lend \n");
	  else
	    fprintf (f, "\n");
	}
	level -= 2;
	indent (f, level);
	fprintf (f, "\\end {cases}");
	break;
      }
    case CASE_EXPR:
      {
	print_expression (f, TREE_OPERAND (exp, 0));
	fprintf (f, " & ");
	print_expression (f, TREE_OPERAND (exp, 1));
	break;
      }
    case MATRIX_EXPR:
      {
	struct tree_list_element *tle = NULL;
	struct tree_list_element *tle2 = NULL;
	fprintf (f, "\\begin{tmatrix}\n");
	level += 2;
	DL_FOREACH (TREE_LIST (TREE_OPERAND (exp, 0)), tle)
	{
	  indent (f, level);
	  DL_FOREACH (TREE_LIST (tle->entry), tle2)
	  {
	    print_expression (f, tle2->entry);
	    fprintf (f, " & ");
	  }
	  fprintf (f, "\\lend\n");
	}
	level -= 2;
	indent (f, level);
	fprintf (f, "\\end{tmatrix}");
	break;
      }
    case CONVERT_EXPR:
      {
	fprintf(f, "<convertion_marker> ");
	print_expression (f, TREE_OPERAND (exp, 0));
	break;
      }
    case OTHERWISE_EXPR:
      fprintf (f, "\\otherwise");
      break;
    case EXPR_MATCH:
      fprintf (f, "\\expr { %d }", TREE_ARG (exp));
      break;
    default:
      {
	const char *opcode;
	switch (TREE_CODE (exp))
	  {
	  case PLUS_EXPR:
	    opcode = "+";
	    break;
	  case MINUS_EXPR:
	    opcode = "-";
	    break;
	  case MULT_EXPR:
	    opcode = "\\cdot";
	    break;
	  case MOD_EXPR:
	    opcode = "\\mod";
	    break;
	  case EQ_EXPR:
	    opcode = "=";
	    break;
	  case GT_EXPR:
	    opcode = ">";
	    break;
	  case GE_EXPR:
	    opcode = "\\geq";
	    break;
	  case LT_EXPR:
	    opcode = "<";
	    break;
	  case LE_EXPR:
	    opcode = "\\leq";
	    break;
	  case NE_EXPR:
	    opcode = "\\neq";
	    break;
	  case LOR_EXPR:
	    opcode = "\\cup";
	    break;
	  case LAND_EXPR:
	    opcode = "\\cap";
	    break;
	  case BOR_EXPR:
	    opcode = "\\lor";
	    break;
	  case BAND_EXPR:
	    opcode = "\\land";
	    break;
	  case XOR_EXPR:
	    opcode = "\\oplus";
	    break;
	  case SRIGHT_EXPR:
	    opcode = "\\ll";
	    break;
	  case SLEFT_EXPR:
	    opcode = "\\gg";
	    break;
	  case ASSIGN_STMT:
	    opcode = "\\gets";
	    break;
	  case DECLARE_STMT:
	    opcode = "\\in";
	    break;
	  case LOWER:
	    opcode = "_";
	    break;
	  case GENERATOR:
	    opcode = ":";
	    break;
	  default:
	    unreachable (0);
	  }

	print_expression (f, TREE_OPERAND (exp, 0));
	fprintf (f, " %s ", opcode);
	print_expression (f, TREE_OPERAND (exp, 1));
	break;
      }
    }

  if (options.print_types)
    {
      if (TREE_CODE_TYPED (TREE_CODE (exp)))
	print_type (f, TREE_TYPE (exp));
      fprintf (f, "}");
    }

  return 0;
}

int
print_type (FILE * f, tree t)
{
  fprintf (f, " type: {");
  if (t != NULL)
    {
      assert (TREE_CODE_CLASS (TREE_CODE (t)) == tcl_type,
	      "tree is not a type");

      fprintf (f, "code: %s, ", TREE_CODE_NAME (TREE_CODE (t)));
      fprintf (f, "size: %zu", TYPE_SIZE (t));
      if (TYPE_DIM (t) != NULL)
	{
	  fprintf (f, ", dim: {");
	  print_expression (f, TYPE_DIM (t));
	  fprintf (f, "}");
	}
      if (TYPE_SHAPE (t) != NULL)
	{
	  fprintf (f, ", shape: {");
	  print_expression (f, TYPE_SHAPE (t));
	  fprintf (f, "}");
	}
    }
  fprintf (f, "}");
  return 0;
}
