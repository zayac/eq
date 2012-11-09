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

#include "eq.h"
#include "tree.h"
#include "print.h"
#include "global.h"

static int level = 0;

#define indent(xf, x)			    \
    do {				    \
        int __i;			    \
        for (__i = 0; __i < (x); __i++)	    \
          xfile_fprintf (xf, "  ");	    \
    } while (0)

int
print_expression (xfile *  xf, tree exp)
{
  if (exp == NULL)
    return 0;
  assert (TREE_CODE (exp) == FUNCTION || TREE_CODE (exp) == LIST
	      || TREE_CODE_CLASS (TREE_CODE (exp)) == tcl_type
	      || TREE_CODE_CLASS (TREE_CODE (exp)) == tcl_expression
	      || TREE_CODE_CLASS (TREE_CODE (exp)) == tcl_constant
	      || TREE_CODE (exp) == IDENTIFIER
	      || TREE_CODE (exp) == ERROR_MARK
	      || TREE_CODE (exp) == IF_STMT
	      || TREE_CODE (exp) == DECLARE_STMT
	      || TREE_CODE (exp) == ASSIGN_STMT
	      || TREE_CODE (exp) == RETURN_STMT
	      || TREE_CODE (exp) == PRINT_MARK,
	  "attempt to print non-expression tree %s",
	  TREE_CODE_NAME (TREE_CODE (exp)));

  if (options.print_types)
    xfile_fprintf (xf, "{");

  switch (TREE_CODE (exp))
    {
    case ERROR_MARK:
      xfile_fprintf (xf, "<<ERROR>>");
      return 0;
    case STRING_CST:
      xfile_fprintf (xf, "%s", TREE_STRING_CST (exp));
      break;
    case REAL_CST:
      xfile_fprintf (xf, "%f", TREE_REAL_CST (exp));
      break;
    case INTEGER_CST:
      xfile_fprintf (xf, "%i", TREE_INTEGER_CST (exp));
      break;
    case LIST:
      {
	struct tree_list_element *tle = NULL;
	DL_FOREACH (TREE_LIST (exp), tle)
	{
	  print_expression (xf, tle->entry);
	  if (tle->next != NULL)
	    xfile_fprintf (xf, ", ");
	}
	break;
      }
    case IDENTIFIER:
      print_expression (xf, TREE_ID_NAME (exp));
      break;
    case FUNCTION_CALL:
      {
	xfile_fprintf (xf, "\\call{");
	print_expression (xf, TREE_OPERAND (exp, 0));
	xfile_fprintf (xf, "}{");
	if (TREE_OPERAND (exp, 1) != NULL)
	  print_expression (xf, TREE_OPERAND (exp, 1));
	xfile_fprintf (xf, "}");
	break;
      }
    case DIV_EXPR:
      {
	xfile_fprintf (xf, "\\frac{");
	print_expression (xf, TREE_OPERAND (exp, 0));
	xfile_fprintf (xf, "}{");
	print_expression (xf, TREE_OPERAND (exp, 1));
	xfile_fprintf (xf, "}");
	break;
      }
    case GENAR_EXPR:
      {
	xfile_fprintf (xf, "\\genar \\limits ^ { ");
	print_expression (xf, TREE_OPERAND (exp, 0));
	xfile_fprintf (xf, " } ( ");
	print_expression (xf, TREE_OPERAND (exp, 1));
	xfile_fprintf (xf, " ) ");
	break;
      }
    case CIRCUMFLEX:
      {
	print_expression (xf, TREE_OPERAND (exp, 0));
	xfile_fprintf (xf, "^{");
	if (TREE_CIRCUMFLEX_INDEX_STATUS (exp))
	  xfile_fprintf (xf, "[");
	print_expression (xf, TREE_OPERAND (exp, 1));
	if (TREE_CIRCUMFLEX_INDEX_STATUS (exp))
	  xfile_fprintf (xf, "]");
	xfile_fprintf (xf, "}");
	break;
      }
    case FUNCTION:
      {
	struct tree_list_element *tle = NULL;
	assert (TREE_CODE (TREE_FUNC_INSTRS (exp)) == LIST, 0);
	xfile_fprintf (xf, "\\begin{ eqcode }{");
	print_expression (xf, TREE_FUNC_NAME (exp));
	xfile_fprintf (xf, "}{");
	if (TREE_FUNC_ARGS (exp) != NULL)
	  print_expression (xf, TREE_FUNC_ARGS (exp));
	xfile_fprintf (xf, "}{");
	if (TREE_FUNC_ARG_TYPES (exp) != NULL)
	  print_expression (xf, TREE_FUNC_ARG_TYPES (exp));
	xfile_fprintf (xf, "}{");
	print_expression (xf, TREE_FUNC_RET_TYPE (exp));
	xfile_fprintf (xf, "}\n");
	level += 2;
	DL_FOREACH (TREE_LIST (TREE_FUNC_INSTRS (exp)), tle)
	{
	  indent (xf, level);
	  print_expression (xf, tle->entry);
	  xfile_fprintf (xf, " \\lend\n");
	}
	level -= 2;
	xfile_fprintf (xf, "\\end{eqcode}\n");
	break;
      }
    case IF_STMT:
      {
	xfile_fprintf (xf, "\\qif { ");
	print_expression (xf, TREE_OPERAND (exp, 0));
	xfile_fprintf (xf, " }\n");
	level += 2;
	indent (xf, level);
	print_expression (xf, TREE_OPERAND (exp, 1));
	xfile_fprintf (xf, "\n");
	level -= 2;
	indent (xf, level);
	if (TREE_OPERAND (exp, 2) != NULL)
	  {
	    xfile_fprintf (xf, "\\else\n");
	    level += 2;
	    indent (xf, level);
	    print_expression (xf, TREE_OPERAND (exp, 2));
	    level -= 2;
	    indent (xf, level);
	  }
	xfile_fprintf (xf, "\\qendif\n");
	break;
      }
    case B_TYPE:
    case N_TYPE:
    case Z_TYPE:
    case R_TYPE:
      {
	if (TYPE_IS_STREAM (exp))
	  xfile_fprintf (xf, "\\overline {");
	xfile_fprintf (xf, "\\type {");
	if (TREE_CODE (exp) == B_TYPE)
	  xfile_fprintf (xf, "B");
	else if (TREE_CODE (exp) == N_TYPE)
	  xfile_fprintf (xf, "N");
	else if (TREE_CODE (exp) == Z_TYPE)
	  xfile_fprintf (xf, "Z");
	else
	  xfile_fprintf (xf, "R");

	if (TYPE_DIM (exp) != NULL)
	  {
	    xfile_fprintf (xf, "^{");
	    print_expression (xf, TYPE_DIM (exp));
	    xfile_fprintf (xf, "}");
	    if (TYPE_SHAPE (exp) != NULL)
	      {
		xfile_fprintf (xf, "_{");
		print_expression (xf, TYPE_SHAPE (exp));
	      }
	    else
	      return xfile_fprintf (xf, "}");
	  }
	xfile_fprintf (xf, "}");
	if (TYPE_IS_STREAM (exp))
	  xfile_fprintf (xf, "}");
	break;
      }
    case FUNCTION_TYPE:
      {
	struct tree_list_element *el;
	xfile_fprintf (xf, "(");
	DL_FOREACH (TREE_LIST (TYPE_FUNCTION_ARGS (exp)), el)
	  {
	    print_expression (xf, el->entry);
	    if (el->next != NULL)
	      xfile_fprintf (xf, ", ");
	  }
	xfile_fprintf (xf, " -> ");
	DL_FOREACH (TREE_LIST (TYPE_FUNCTION_RET (exp)), el)
	  {
	    print_expression (xf, el->entry);
	    if (el->next != NULL)
	      xfile_fprintf (xf, ", ");
	  }
	xfile_fprintf (xf, ")");
	break;
      }
    case FILTER_EXPR:
      {
	xfile_fprintf (xf, "\\filter { ");
	print_expression (xf, TREE_OPERAND (exp, 0));
	xfile_fprintf (xf, " | ");
	print_expression (xf, TREE_OPERAND (exp, 1));
	xfile_fprintf (xf, " } ");
	break;
      }
    case UMINUS_EXPR:
      {
	xfile_fprintf (xf, " -");
	print_expression (xf, TREE_OPERAND (exp, 0));
	break;
      }
    case NOT_EXPR:
      {
	xfile_fprintf (xf, "\\lnot ");
	print_expression (xf, TREE_OPERAND (exp, 0));
	break;
      }
    case RETURN_STMT:
      {
	xfile_fprintf (xf, "\\return {");
	print_expression (xf, TREE_OPERAND (exp, 0));
	xfile_fprintf (xf, "} ");
	break;
      }
    case PRINT_MARK:
      {
	xfile_fprintf (xf, "\\print {");
	print_expression (xf, TREE_OPERAND (exp, 0));
	xfile_fprintf (xf, "} ");
	break;
      }
    case FORALL:
      {
	xfile_fprintf (xf, "\\forall ");
	print_expression (xf, TREE_OPERAND (exp, 0));
	break;
      }
    case PARALLEL_LOOP_EXPR:
      {
	struct tree_list_element *tle = NULL;
	print_expression (xf, TREE_OPERAND (exp, 0));
	xfile_fprintf (xf, " | ");
	print_expression (xf, TREE_OPERAND (exp, 1));
	xfile_fprintf (xf, " = ");
	xfile_fprintf (xf, "\\begin{cases}\n");
	level += 2;
	DL_FOREACH (TREE_LIST (TREE_OPERAND (exp, 2)), tle)
	{
	  indent (xf, level);
	  print_expression (xf, tle->entry);
	  if (tle->next != NULL)
	    xfile_fprintf (xf, " \\lend \n");
	  else
	    xfile_fprintf (xf, "\n");
	}
	level -= 2;
	indent (xf, level);
	xfile_fprintf (xf, "\\end {cases}");
	break;
      }
    case CASE_EXPR:
      {
	print_expression (xf, TREE_OPERAND (exp, 0));
	xfile_fprintf (xf, " & ");
	print_expression (xf, TREE_OPERAND (exp, 1));
	break;
      }
    case MATRIX_EXPR:
      {
	struct tree_list_element *tle = NULL;
	struct tree_list_element *tle2 = NULL;

	xfile_fprintf (xf, "\\begin{tmatrix}\n");
	level += 2;
	DL_FOREACH (TREE_LIST (TREE_OPERAND (exp, 0)), tle)
	{
	  indent (xf, level);
	  DL_FOREACH (TREE_LIST (tle->entry), tle2)
	  {
	    print_expression (xf, tle2->entry);
	    xfile_fprintf (xf, " & ");
	  }
	  xfile_fprintf (xf, "\\lend\n");
	}
	level -= 2;
	indent (xf, level);
	xfile_fprintf (xf, "\\end{tmatrix}");
	break;
      }
    case CONVERT_EXPR:
      {
	xfile_fprintf (xf, "<convertion_marker> ");
	print_expression (xf, TREE_OPERAND (exp, 0));
	break;
      }
    case OTHERWISE_EXPR:
      xfile_fprintf (xf, "\\otherwise");
      break;
    case EXPR_MATCH:
      xfile_fprintf (xf, "\\expr { %d }", TREE_ARG (exp));
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

	print_expression (xf, TREE_OPERAND (exp, 0));
	xfile_fprintf (xf, " %s ", opcode);
	print_expression (xf, TREE_OPERAND (exp, 1));
	break;
      }
    }

  if (options.print_types)
    {
      if (TREE_CODE_TYPED (TREE_CODE (exp)))
	print_type (xf, TREE_TYPE (exp));
      xfile_fprintf (xf, "}");
    }

  return 0;
}

int
print_type (xfile *  xf, tree t)
{
  xfile_fprintf (xf, " type: {");
  if (t != NULL)
    {
      assert (TREE_CODE_CLASS (TREE_CODE (t)) == tcl_type,
	      "tree is not a type");

      xfile_fprintf (xf, "code: %s, ", TREE_CODE_NAME (TREE_CODE (t)));
      xfile_fprintf (xf, "size: %zu", TYPE_SIZE (t));
      if (TYPE_DIM (t) != NULL)
	{
	  xfile_fprintf (xf, ", dim: {");
	  print_expression (xf, TYPE_DIM (t));
	  xfile_fprintf (xf, "}");
	}
      if (TYPE_SHAPE (t) != NULL)
	{
	  xfile_fprintf (xf, ", shape: {");
	  print_expression (xf, TYPE_SHAPE (t));
	  xfile_fprintf (xf, "}");
	}
    }
  xfile_fprintf (xf, "}");
  return 0;
}
