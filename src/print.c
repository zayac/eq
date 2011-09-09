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
            TREE_CODE_NAME (TREE_CODE (exp)));*/
  //printf("%s\n", TREE_CODE_NAME(TREE_CODE(exp)));
      assert (exp != NULL 
          && (   TREE_CODE(exp) == FUNCTION
	      || TREE_CODE(exp) == LIST
              || TREE_CODE_CLASS (TREE_CODE (exp)) == tcl_type
              || TREE_CODE_CLASS (TREE_CODE (exp)) == tcl_expression
              || TREE_CODE_CLASS (TREE_CODE (exp)) == tcl_constant
              || TREE_CODE (exp) == IDENTIFIER
              || TREE_CODE (exp) == ERROR_MARK
	      || TREE_CODE (exp) == IF_STMT),
          "attempt to print non-expression tree %s",
          TREE_CODE_NAME (TREE_CODE (exp)));
  
  switch (TREE_CODE (exp))
    {
    case ERROR_MARK:
      return fprintf (f, "<<ERROR>>");
    case STRING_CST:
      return fprintf (f, "%s", TREE_STRING_CST (exp));
    case INTEGER_CST:
      return fprintf (f, "%i", TREE_INTEGER_CST (exp));
    case LIST:
      {
	struct tree_list_element * tle = NULL;
	DL_FOREACH (TREE_LIST (exp), tle)
	  {
	    print_expression (f, tle->entry);
	    if (tle->next != NULL)
	      fprintf (f, ", ");
	  }
	  return 0;
      }
    case IDENTIFIER:
      return print_expression (f, TREE_ID_NAME (exp));
    case FUNCTION_CALL:
      {
	fprintf (f, "\\call{");
	print_expression (f, TREE_OPERAND (exp, 0));
	fprintf (f, "}{");
	if (TREE_OPERAND (exp, 1) != NULL)
	  print_expression (f, TREE_OPERAND (exp, 1));
	return fprintf (f, "}");
      }
    case DIV_EXPR:
      {
	fprintf (f, "\\frac{");
	print_expression (f, TREE_OPERAND (exp, 0));
	fprintf (f, "}{");
	print_expression (f, TREE_OPERAND (exp, 1));
	return fprintf (f, "}");
      }
    case GENAR_EXPR:
      {
	fprintf(f, "\\genar \\limits ^ { ");
	print_expression (f, TREE_OPERAND (exp, 0));
	fprintf(f, " } ( ");
	print_expression (f, TREE_OPERAND (exp, 1));
	return fprintf(f, " ) ");
      }
    case VECTOR_EXPR:
      {
	struct tree_list_element * tle = NULL;
	fprintf(f, "\\begin { tvector }\n");
	level += 2;
	DL_FOREACH (TREE_LIST (TREE_OPERAND (exp, 0)), tle) 
	  {
	    indent (f, level);
	    print_expression (f, tle->entry);
	    fprintf(f, " \\lend\n");
	  }
	level -=2;
	return fprintf(f, "\\end{tvector}");
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
	return fprintf (f, "}");
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
	DL_FOREACH (TREE_LIST (TREE_FUNC_INSTRS(exp)), tle)
	  {
	    indent (f, level);
	    print_expression (f, tle->entry);
	    fprintf (f, " \\lend\n");
	  }
	level -= 2;
	return fprintf (f, "\\end{eqcode}\n");
      }
    case IF_STMT:
      {
	struct tree_list_element *tle = NULL;
	int index;
	DL_FOREACH (TREE_LIST (TREE_OPERAND (exp, 0)), tle)
	  {
	    struct tree_list_element *instr = NULL;
	    if (TREE_CODE (tle->entry) == ELSE_STMT)
	      fprintf (f, "\\qelse\n");
	    else
	      {
		/* check if the element is the first in the list.  */
		if (tle == TREE_LIST (TREE_OPERAND (exp, 0)))
		  fprintf (f, "\\qif { ");
		else
		  fprintf (f, "\\qifelse { ");
		print_expression (f, TREE_OPERAND (tle->entry, 0));
		fprintf (f, " }\n");
	      }
	    level += 2;
	    if (TREE_CODE (tle->entry) == ELSE_STMT)
	      index = 0;
	    else
	      index = 1;
	    DL_FOREACH (TREE_LIST (TREE_OPERAND (tle->entry, index)), instr)
	      {
		indent (f, level);
		print_expression (f, instr->entry);
		printf(" \\lend\n");
	      }
	    level -= 2;
	    indent (f, level);
	  }
	return fprintf(f, "\\qendif");
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

	if (TREE_TYPE_DIM (exp) != NULL)
	  {
	    fprintf (f, "^{");
	    print_expression (f, TREE_TYPE_DIM (exp));
	    fprintf (f, "}");
	    if (TREE_TYPE_SHAPE (exp) != NULL)
	      {
		fprintf (f, "_{");
		print_expression (f, TREE_TYPE_SHAPE (exp));
	      }
	    else
	      return fprintf (f, "}");
	  }
	return fprintf (f, "}");
      }
    case FILTER_EXPR:
      {
	fprintf (f, "\\filter { ");
	print_expression (f, TREE_OPERAND (exp, 0));
	fprintf (f, " | ");
	print_expression (f, TREE_OPERAND (exp, 1));
	return fprintf (f, " } ");
      }
    case UMINUS_EXPR:
      fprintf (f, " -");
      return print_expression (f, TREE_OPERAND (exp, 0));
    case NOT_EXPR:
      fprintf (f, "\\lnot ");
      return print_expression (f, TREE_OPERAND (exp, 0));
    case RETURN_EXPR:
      fprintf (f, "\\return {");
      print_expression (f, TREE_OPERAND (exp, 0));
      return fprintf (f, "} ");
    case FORALL:
      fprintf (f, "\\forall ");
      return print_expression (f, TREE_OPERAND (exp, 0));
    case WITH_LOOP_EXPR:
      {
	struct tree_list_element *tle = NULL;
	print_expression (f, TREE_OPERAND (exp, 0));
	fprintf (f, " | ");
	print_expression (f, TREE_OPERAND (exp, 1));
	fprintf (f, " = ");
	fprintf (f, "\\begin{cases}\n");
	level += 2;
	DL_FOREACH(TREE_LIST(TREE_OPERAND(exp, 2)), tle)
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
	return 0;
      }
    case CASE_EXPR:
      print_expression (f, TREE_OPERAND (exp, 0));
      fprintf (f, " & ");
      return print_expression (f, TREE_OPERAND (exp, 1));
    case OTHERWISE_EXPR:
      return fprintf (f, "\\otherwise");
    case EXPR_MATCH:
      return fprintf(f, "\\expr { %d }", TREE_ARG (exp));
    default:
      {
	const char *opcode;
	int ret = 0;
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
	    opcode = ">=";
	    break;
	  case LT_EXPR:
	    opcode = "<";
	    break;
	  case LE_EXPR:
	    opcode = "<=";
	    break;
	  case NE_EXPR:
	    opcode = "!=";
	    break;
	  case OR_EXPR:
	    opcode = "\\lor";
	    break;
	  case AND_EXPR:
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
	  case ASSIGN_EXPR:
	    opcode = "\\gets";
	    break;
	  case DECLARE_EXPR:
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
	/* for the time being in parens.  */
	ret += print_expression (f, TREE_OPERAND (exp, 0));
	fprintf (f, " %s ", opcode);
	ret += print_expression (f, TREE_OPERAND (exp, 1));
	return ret;
      }
    }
}
