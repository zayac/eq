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
#include "utarray.h"

#include "expand.h"

enum tree_code_class
{
  tcl_misc,
  tcl_type,
  tcl_constant,
  tcl_expression
};

#define DEF_TREE_CODE(code, desc, class, operands, typed) code,
enum tree_code
{
#include "tree.def"
};
#undef DEF_TREE_CODE

extern enum tree_code_class tree_code_type[];
#define TREE_CODE_CLASS(code) tree_code_type[(int) (code)]

extern unsigned char tree_code_operand[];
#define TREE_CODE_OPERANDS(code) tree_code_operand[(int) (code)]

extern bool tree_code_typed[];
#define TREE_CODE_TYPED(code) tree_code_typed[(int) (code)]
extern const char *tree_code_name[];
#define TREE_CODE_NAME(code) tree_code_name[(int) (code)]

union tree_node;
typedef union tree_node *tree;

/* Basic information each node should have.  */
struct tree_base
{
  enum tree_code code;
  struct location loc;
};

/* Base tree with operands pointer */
struct tree_base_op
{
  struct tree_base base;
  tree operands[];
};

/* Basic information each typed node should have.  */
struct tree_type_base
{
  struct tree_base base;
  tree type;
  unsigned int is_constant:1;
};

struct tree_type_node
{
  struct tree_base base;
  tree name;
  tree size;
};

struct tree_type_base_op
{
  struct tree_type_base typed;
  tree operands[];
};

struct tree_list_node
{
  struct tree_type_base typed;
  UT_array *list;
};

struct tree_circumflex_op_node
{
  struct tree_base base;
  bool is_index;
  tree operands[];
};

struct tree_string_cst_node
{
  struct tree_type_base typed;
  int length;
  char *value;
};

struct tree_int_cst_node
{
  struct tree_type_base typed;
  int value;
};

struct tree_identifier_node
{
  struct tree_type_base typed;
  tree name;
};

union tree_node
{
  struct tree_base base;
  struct tree_base_op base_op;
  struct tree_type_base typed;
  struct tree_type_base_op typed_op;
  struct tree_type_node type_node;
  struct tree_identifier_node identifier_node;
  struct tree_list_node list_node;
  struct tree_int_cst_node int_cst_node;
  struct tree_string_cst_node string_cst_node;
  struct tree_circumflex_op_node circumflex_op_node;
};

enum tree_global_code
{
  TG_ERROR_MARK,
  TG_B_TYPE,
  TG_N_TYPE,
  TG_Z_TYPE,
  TH_R_TYPE,
  TG_MAX
};

#define error_mark_node     global_tree[TG_ERROR_MARK]
#define b_type_node         global_tree[TG_B_TYPE]
#define n_type_node         global_tree[TG_N_TYPE]
#define z_type_node         global_tree[TG_Z_TYPE]
#define r_type_node         global_tree[TG_R_TYPE]

#define TREE_LIST(node) ((node)->list_node.list)
#define TREE_CODE(node) ((enum tree_code) (node)->base.code)
#define TREE_LOCATION(node) ((node)->base.loc)
#define TREE_CODE_SET(node, value) ((node)->base.code = (value))

/* Checks if it is possible to access the operand number IDX
   in the node with the code CODE.  */
static inline bool
tree_operand_in_range (enum tree_code code, const int idx)
{
  return idx >= 0 && idx <= TREE_CODE_OPERANDS (code) - 1;
}

/* Returns the operand of the NODE at index IDX checking
   that index is in the range of the node.  */
static inline tree
get_tree_operand (tree node, int idx)
{
  enum tree_code code = TREE_CODE (node);

  assert (tree_operand_in_range (code, idx),
	  "operand index out of range or no operands in the node");

  if (TREE_CODE_OPERANDS (code) > 0)
    {
      if (code == CIRCUMFLEX)
	return node->circumflex_op_node.operands[idx];
      if (TREE_CODE_TYPED (code))
	return node->typed_op.operands[idx];
      else
	return node->base_op.operands[idx];
    }
  else
    unreachable ("node `%s` doesn't have operands", TREE_CODE_NAME (code));
}

static inline void
set_tree_operand (tree node, int idx, tree value)
{
  enum tree_code code = TREE_CODE (node);
  assert (tree_operand_in_range (code, idx),
	  "operand index out of range or no operands in the node");

  if (TREE_CODE_OPERANDS (code) > 0)
    {
      if (code == CIRCUMFLEX)
	node->circumflex_op_node.operands[idx] = value;
      if (TREE_CODE_TYPED (code))
	node->typed_op.operands[idx] = value;
      else
	node->base_op.operands[idx] = value;
    }
  else
    unreachable ("nod `%s` does not have operands", TREE_CODE_NAME (code));
}

#define TREE_OPERAND(node, i) get_tree_operand ((node), (i))
#define TREE_OPERAND_SET(node, i, value) set_tree_operand ((node), (i), (value))

#define TREE_INTEGER_CST(node) ((node)->int_cst_node.value)
#define TREE_STRING_CST(node) ((node)->string_cst_node.value)
#define TREE_STRING_CST_LENGTH(node) ((node)->string_cst_node.length)

#define TREE_ID_NAME(node) ((node)->identifier_node.name)

#define TREE_TYPE_DIM(node) ((node)->typed_op.operands[0])
#define TREE_TYPE_SHAPE(node) ((node)->typed_op.operands[1])

#define TREE_FUNC_NAME(node) ((node)->typed_op.operands[0])
#define TREE_FUNC_ARGS(node) ((node)->typed_op.operands[1])
#define TREE_FUNC_ARGS_TYPES(node) ((node)->typed_op.operands[2])
#define TREE_FUNC_RET_TYPE(node) ((node)->typed_op.operands[3])
#define TREE_FUNC_INSTRS(node) ((node)->typed_op.operands[4])

#define TREE_CIRCUMFLEX_INDEX_STATUS(node) ((node)->circumflex_op_node.is_index)
static inline bool
is_assignment_operator (enum token_kind tk)
{
  switch (tk)
    {
    case tv_gets:
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
tree make_function (tree, tree, tree, tree, tree, struct location);
tree make_type (enum tree_code);
tree make_binary_op (enum tree_code, tree, tree);
tree make_unary_op (enum tree_code, tree, struct location);
tree make_matrix (tree, tree, struct location);
tree make_vector (tree, struct location);
tree make_genar (tree, tree, struct location);
tree make_with_loop (tree, tree, tree, bool);
tree make_return (tree, struct location);
tree make_assign (enum token_kind, tree, tree);
tree tree_list_copy (tree);
void free_list (tree);

#endif /* __TREE_H__  */
