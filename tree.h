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

#ifndef __TREE_H__
#define __TREE_H__

#include <stdlib.h>
#include <sys/queue.h>

#include "expand.h"

enum tree_code_class
{
  tcl_misc,
  tcl_type,
  tcl_constant,
  tcl_statement,
  tcl_expression
};

#define DEF_TREE_CODE(code, desc, class, operands) code,
enum tree_code 
{
#include "tree.def"
};
#undef DEF_TREE_CODE

extern enum tree_code_class tree_code_type[];
#define TREE_CODE_CLASS(code) tree_code_type[(int) (code)]

extern enum tree_code_class tree_code_operand[];
#define TREE_CODE_OPERANDS(code) tree_code_operand[(int) (code)]

extern const char * tree_code_name[];
#define TREE_CODE_NAME(code) tree_code_name[(int) (code)]

union tree_node;
typedef union tree_node *tree;

/* Basic information each node should have.  */
struct tree_base
{
  enum tree_code code;
  struct location loc;
};

/* Basic information each type should have.  */
struct tree_type_base
{
  struct tree_base base;
  tree type;
  unsigned int is_constant:1;
};

/* Structure to store a list of tree nodes.  */
struct tree_list_element
{
  tree                              element;
  TAILQ_ENTRY (tree_list_element)   entries;
};

TAILQ_HEAD (tree_list, tree_list_element);

struct tree_list_node
{
  //struct tree_base base;
  struct tree_type_base typed;
  struct tree_list list;
};

struct tree_type_node
{
  struct tree_base base;
  tree name;
  tree size;
};

struct tree_function_type_node
{
  struct tree_base base;
  tree name;
  tree operands[2];
};

struct tree_documentclass_node
{
  struct tree_base base;
  tree operands[4];
};

struct tree_usepackage_node
{
  struct tree_base base;
  tree operands[1];
};

struct tree_begin_node
{
  struct tree_base base;
  tree operands[1];
};

struct tree_end_node
{
  struct tree_base base;
  tree operands[1];
};

struct tree_string_cst_node
{
  //struct tree_base base;
  struct tree_type_base typed;
  int  length;
  char *  value;
};

struct tree_int_cst_node
{
  //struct tree_base base;
  struct tree_type_base typed;
  int value;
};

struct tree_list_cst_node
{
  //struct tree_base base;
  struct tree_type_base typed;
  int size;
  tree elements;
};

struct tree_identifier_node
{
  //struct tree_base base;
  struct tree_type_base typed;
  tree name;
};

struct tree_unary_expr_node
{
  //struct tree_base base;
  struct tree_type_base typed;
  tree operands[1];
};

struct tree_binary_expr_node
{
  //struct tree_base base;
  struct tree_type_base typed;
  tree operands[2];
};

struct tree_trinary_expr_node
{
  //struct tree_base base;
  struct tree_type_base typed;
  tree operands[3];
};


struct tree_three_op_stmt_node
{
  struct tree_base base;
  tree operands[3];
};

struct tree_stmt_list_node
{
  struct tree_base base;
  tree stmts;
  tree vars;
};

/*struct tree_one_op_stmt_node
{
  struct tree_base base;
  tree name;
};*/



union tree_node
{
  struct tree_base                  base;
  struct tree_type_base             typed;
  struct tree_list_node             list_node;
  struct tree_type_node             type_node;
  struct tree_function_type_node    function_type_node;
  struct tree_string_cst_node       string_cst_node;
  struct tree_int_cst_node          int_cst_node;
  struct tree_list_cst_node         list_cst_node;
  struct tree_identifier_node       identifier_node;
  struct tree_unary_expr_node       unary_expr_node;
  struct tree_binary_expr_node      binary_expr_node;
  struct tree_trinary_expr_node     trinary_expr_node;
  struct tree_three_op_stmt_node    three_op_stmt_node;
  struct tree_stmt_list_node        stmt_list_node;
  struct tree_documentclass_node    documentclass_node;
  struct tree_usepackage_node       usepackage_node;
  struct tree_begin_node            begin_node;
  struct tree_end_node              end_node;
  /*struct tree_one_op_stmt_node      one_op_stmt_node;*/
};


extern tree global_tree[];

enum tree_global_code
{
  TG_ERROR_MARK,
  TG_INTEGER_TYPE,
  TG_STRING_TYPE,
  TG_LIST_TYPE,
  TG_VOID_TYPE,
  TG_MAX
};

#define error_mark_node     global_tree[TG_ERROR_MARK]
#define integer_type_node   global_tree[TG_INTEGER_TYPE]
#define string_type_node    global_tree[TG_STRING_TYPE]
#define list_type_node      global_tree[TG_LIST_TYPE]
#define void_type_node      global_tree[TG_VOID_TYPE]

#define TREE_CODE(node) ((enum tree_code) (node)->base.code)
#define TREE_LOCATION(node) ((node)->base.loc)
#define TREE_CODE_SET(node, value) ((node)->base.code = (value))

#define TREE_TYPE(node) ((node)->typed.type)
#define TREE_TYPE_NAME(node)  ((node)->type_node.name)
#define TREE_CONSTANT(node) ((node)->typed.is_constant)

/* Checks if it is possible to access the operand number IDX
   in the node with the code CODE.  */
static inline bool
tree_operand_in_range (enum tree_code code, const int idx)
{
  return idx >= 0 && idx <= TREE_CODE_OPERANDS (code) -1;
}

/* Returns the operand of the NODE at index IDX checking
   that index is in the range of the node.  */
static inline tree
get_tree_operand (tree node, int idx)
{
  enum tree_code code = TREE_CODE (node);
  
  assert (tree_operand_in_range (code, idx), 
          "operand index out of range or no operands in the node");
  switch (TREE_CODE_CLASS (code))
    {
    case tcl_misc:
      if (code == DOCUMENTCLASS)
        return node->documentclass_node.operands[idx];
      else if (code == USEPACKAGE)
        return node->usepackage_node.operands[idx];
      else if (code == BEGIN)
        return node->begin_node.operands[idx];
      else if (code == END)
        return node->end_node.operands[idx];
      else
        unreachable("node `%s` of tcl_misc doesn't have operands", TREE_CODE_NAME (code));
      break;
    case tcl_type:
      if (code == FUNCTION_TYPE)
        return node->function_type_node.operands[idx];
      else
        unreachable ("node `%s' of tcl_type do not have operands", 
                     TREE_CODE_NAME (code));
      break;

    case tcl_statement:
      return node->three_op_stmt_node.operands[idx];

    case tcl_expression:
      if (code == UMINUS_EXPR || code == TRUTH_NOT_EXPR)
        return node->unary_expr_node.operands[idx];
      else
        return node->binary_expr_node.operands[idx];

    default:  
      unreachable ("node %s does not have operands!",
                   TREE_CODE_NAME (code));
    }
}


static inline void
set_tree_operand (tree node, int idx, tree value)
{
  enum tree_code code = TREE_CODE (node);
  assert (tree_operand_in_range (code, idx), 
          "operand index out of range or no operands in the node");
  switch (TREE_CODE_CLASS (code))
    {
    case tcl_misc:
      if (code == DOCUMENTCLASS)
        node->documentclass_node.operands[idx] = value;
      else if (code == USEPACKAGE)
        node->usepackage_node.operands[idx] = value;
      else if (code == BEGIN)
        node->begin_node.operands[idx] = value;
      else if (code == END)
        node->end_node.operands[idx] = value;
      else
        unreachable ("nod `%s` of tcl_misc does not have operands",
                    TREE_CODE_NAME (code));
      break;
    case tcl_type:
      if (code == FUNCTION_TYPE)
        node->function_type_node.operands[idx] = value;
      else
        unreachable ("node `%s' of tcl_type do not have operands", 
                     TREE_CODE_NAME (code));
      break;

    case tcl_statement:
      node->three_op_stmt_node.operands[idx] = value;
      break;

    case tcl_expression:
      if (code == UMINUS_EXPR || code == TRUTH_NOT_EXPR)
        node->unary_expr_node.operands[idx] = value;
      else
        node->binary_expr_node.operands[idx] = value;
      break;

    default:  
      unreachable ("node %s does not have operands",
                   TREE_CODE_NAME (code));
    }
}


#define TREE_OPERAND(node, i) get_tree_operand ((node), (i))
#define TREE_OPERAND_SET(node, i, value) set_tree_operand ((node), (i), (value))

#define TREE_INTEGER_CST(node) ((node)->int_cst_node.value)
#define TREE_STRING_CST(node) ((node)->string_cst_node.value)
#define TREE_STRING_CST_LENGTH(node) ((node)->string_cst_node.length)
#define TREE_LIST_CST(node) ((node)->list_cst_node.elements)
#define TREE_LIST_CST_LENGTH(node) ((node)->list_cst_node.length)

#define TREE_LIST_QUEUE(node) ((node)->list_node.list)

#define TREE_ID_NAME(node) ((node)->identifier_node.name)

#define TREE_STMT_LIST_STMTS(node) ((node)->stmt_list_node.stmts)
#define TREE_STMT_LIST_VARS(node) ((node)->stmt_list_node.vars)

#define TREE_FUNC_TYPE_NAME(node) ((node)->function_type_node.name)

static inline bool
is_assignment_operator (enum token_kind tk)
{
  switch (tk)
    {
    case tv_eq:
      return true;
    default:
      return false;
    }
}



tree make_tree (enum tree_code);
void free_tree (tree);
void free_atomic_trees ();
tree make_string_cst_tok (struct token *);
tree make_string_cst_str (const char *);
tree make_identifier_tok (struct token *);
tree make_integer_tok (struct token *);
tree make_tree_list ();
bool tree_list_append (tree, tree);
tree make_binary_op (enum tree_code, tree, tree);
tree make_assign (enum token_kind, tree, tree);
tree tree_list_copy (tree);
void free_list (tree);

#endif /* __TREE_H__  */

