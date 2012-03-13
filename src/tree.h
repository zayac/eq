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
#include "eq.h"
#include "utlist.h"
#include "uthash.h"

enum tree_code_class
{
  tcl_misc,
  tcl_type,
  tcl_constant,
  tcl_expression,
  tcl_statement
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
  struct location loc;
  enum tree_code code;
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
  /* These options are needed while parsing \match.  */
  unsigned argset:1;
  unsigned arg:6;
};

struct tree_type_base_op
{
  struct tree_type_base typed;
  tree operands[];
};

struct type_numerical
{
  tree dim;
  tree shape;
};

union type_properties
{
  struct type_numerical numerical;
  tree functional;
};

/* A hash table for storing types.  */
struct tree_type_node
{
  struct tree_base base;
  size_t size;
  union type_properties properties;
  UT_hash_handle hh;
};

struct tree_list_element
{
  tree entry;
  struct tree_list_element *next, *prev;
};

struct tree_list_node
{
  struct tree_base base;
  struct tree_list_element *list;
};

struct tree_circumflex_op_node
{
  struct tree_type_base typed;
  bool is_index;
  tree operands[];
};

struct tree_string_cst_node
{
  struct tree_type_base typed;
  int length;
  bool is_char;
  char *value;
};

struct tree_int_cst_node
{
  struct tree_type_base typed;
  long value;
};

struct tree_real_cst_node
{
  struct tree_type_base typed;
  double value;
};

struct tree_identifier_node
{
  struct tree_type_base typed;
  tree name;
  tree iter_desc;
  unsigned defined:1;
};

struct tree_rec_expr_node
{
  struct tree_base base;
  int min_value;
  unsigned size;
  tree list;
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
  struct tree_real_cst_node real_cst_node;
  struct tree_string_cst_node string_cst_node;
  struct tree_circumflex_op_node circumflex_op_node;
  struct tree_rec_expr_node rec_expr_node;
};

enum tree_global_code
{
  TG_ERROR_MARK,
  TG_UNKNOWN_MARK,
  TG_ITER_VAR,
  TG_B_TYPE,
  TG_N_TYPE,
  TG_Z_TYPE,
  TG_R_TYPE,
  TG_MAX
};

#define error_mark_node     global_tree[TG_ERROR_MARK]
#define unknown_mark_node   global_tree[TG_UNKNOWN_MARK]
#define iter_var_node	    global_tree[TG_ITER_VAR]
#define b_type_node         global_tree[TG_B_TYPE]
#define n_type_node         global_tree[TG_N_TYPE]
#define z_type_node         global_tree[TG_Z_TYPE]
#define r_type_node         global_tree[TG_R_TYPE]

#define TREE_LIST(node) ((node)->list_node.list)
#define TREE_CODE(node) ((enum tree_code) (node)->base.code)
#define TREE_CONSTANT(node) ((node)->typed.is_constant)
#define TREE_LOCATION(node) ((node)->base.loc)
#define TREE_CODE_SET(node, value) ((node)->base.code = (value))

#define TREE_TYPE(node) ((node)->typed.type)
#define TYPE_HASH(node) ((node)->type_node)
#define TYPE_SIZE(node) ((node)->type_node.size)
#define TYPE_DIM(node) ((node)->type_node.properties.numerical.dim)
#define TYPE_SHAPE(node) ((node)->type_node.properties.numerical.shape)
#define TYPE_FUNCTION(node) \
  ((node)->type_node.properties.functional)

#define TREE_ARGSET(node) ((node)->typed.argset)
#define TREE_ARG(node) ((node)->typed.arg)

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
      else if (TREE_CODE_TYPED (code))
	node->typed_op.operands[idx] = value;
      else
	node->base_op.operands[idx] = value;
    }
  else
    unreachable ("node `%s` does not have operands", TREE_CODE_NAME (code));
}

#define TREE_OPERAND(node, i) get_tree_operand ((node), (i))
#define TREE_OPERAND_SET(node, i, value) set_tree_operand ((node), (i), (value))

#define TREE_INTEGER_CST(node) ((node)->int_cst_node.value)
#define TREE_REAL_CST(node)  ((node)->real_cst_node.value)
#define TREE_STRING_CST(node) ((node)->string_cst_node.value)
#define TREE_STRING_CST_LENGTH(node) ((node)->string_cst_node.length)
#define TREE_STRING_CST_IS_CHAR(node) ((node)->string_cst_node.is_char)

#define TREE_ID(node) (&((node)->identifier_node))
#define TREE_ID_NAME(node) ((node)->identifier_node.name)
#define TREE_ID_DEFINED(node) ((node)->identifier_node.defined)
#define TREE_ID_ITER(node) ((node)->identifier_node.iter_desc)

#define TREE_ITER_MIN(node) ((node)->rec_expr_node.min_value)
#define TREE_ITER_SIZE(node) ((node)->rec_expr_node.size)
#define TREE_ITER_LIST(node) ((node)->rec_expr_node.list)

#define TREE_FUNC_NAME(node) ((node)->base_op.operands[0])
#define TREE_FUNC_ARGS(node) ((node)->base_op.operands[1])
#define TREE_FUNC_ARG_TYPES(node) ((node)->base_op.operands[2])
#define TREE_FUNC_RET_TYPE(node) ((node)->base_op.operands[3])
#define TREE_FUNC_INSTRS(node) ((node)->base_op.operands[4])

#define TREE_IS_FUNCTION_PROTO (TREE_FUNC_INSTRS(node) == NULL)

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

size_t get_tree_size (enum tree_code);
tree make_tree (enum tree_code);
void free_tree (tree);
void free_tree_type (tree, bool);
void free_atomic_trees ();
tree make_string_cst_tok (struct token *);
tree make_string_cst_str (const char *);
tree make_identifier_tok (struct token *);
tree make_integer_cst (int);
tree make_integer_tok (struct token *);
tree make_real_tok (struct token *);
tree make_tree_list ();
bool tree_list_append (tree, tree);
tree make_function (tree, tree, tree, tree, tree, struct location);
tree make_type (enum tree_code);
tree make_binary_op (enum tree_code, tree, tree);
tree make_unary_op (enum tree_code, tree, struct location);
tree make_matrix (tree, struct location);
tree make_genar (tree, tree, struct location);
tree make_index_loop (tree, tree, tree, bool);
tree make_return (tree, struct location);
tree make_assign (enum token_kind, tree, tree);
tree make_convert (tree, tree);
tree tree_list_copy (tree);
tree tree_copy (tree);
bool tree_compare (tree, tree);
void tree_list_combine (tree, tree);
void tree_list_split (tree, tree);
void free_list (tree);
tree eliminate_list (tree);
int  equal_list_sizes (tree, tree);
char* tree_to_str (tree);

#endif /* __TREE_H__  */
