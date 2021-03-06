/* Copyright (c) 2011,2012 Artem Shinkarov <artyom.shinkaroff@gmail.com>
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
  struct eq_location loc;
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

struct type_functional
{
  tree arg_types;
  tree ret_types;
};

union type_properties
{
  struct type_numerical numerical;
  struct type_functional functional;
};

/* A hash table for storing types.  */
struct tree_type_node
{
  struct tree_base base;
  size_t size;
  bool is_stream;
  union type_properties properties;
  UT_hash_handle hh;
  tree list;
};

struct tree_stmt_node
{
  struct tree_type_base typed;
  /* A reference to the closest `if' parent statement.  */
  tree parent_if;
  /* Indicate if statement is redundant in terms of reachability from return
     value.  */
  unsigned is_redundant:1;
  /* Set this field to true, if we already performed a redundancy check with
     this node.  */
  unsigned redundance_checked:1;
  unsigned def_number;
  unsigned is_recurrence_def;
  /* Statements which the current statement is dependent on.  */
  tree defs;
  /* Statements which are dependent on the current statement.  */
  tree uses;
  tree operands[];
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

struct tree_function_node
{
  struct tree_type_base typed;
  struct control_flow_graph *cfg;
  /* A list of statements from which function can start execution.  */
  tree entry_stmts;
  /* A list of return statements.  */
  tree return_stmts;
  /* A list of print statements.  */
  tree print_stmts;
  tree schedule;
  tree operands[];
};

struct tree_circumflex_op_node
{
  struct tree_type_base typed;
  bool is_index;
  tree index;
  tree operands[];
};

struct tree_string_cst_node
{
  struct tree_type_base typed;
  int length;
  unsigned is_char:1;
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
  tree source_name;
  tree iter_desc;
  tree iter_def;
  /* Statement with the definition of current variable.  */
  tree def;
  /* a use-definition chain for curent variable.  */
  tree ud_chain;
  /* a definition-use chain for current variable.  */
  tree du_chain;
  unsigned defined:1;
  unsigned with_prefix:1;
};

struct tree_iter_pair
{
  struct tree_base base;
  tree lower_index;
  tree operands[];
};

/* Structure declaration, which is defined in ssa.c.  */
struct tree_hash_node;

struct tree_phi_node
{
  struct tree_type_base typed;
  struct tree_hash_node *values;
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
  struct tree_stmt_node stmt_node;
  struct tree_function_node function_node;
  struct tree_identifier_node identifier_node;
  struct tree_list_node list_node;
  struct tree_int_cst_node int_cst_node;
  struct tree_real_cst_node real_cst_node;
  struct tree_string_cst_node string_cst_node;
  struct tree_circumflex_op_node circumflex_op_node;
  struct tree_rec_expr_node rec_expr_node;
  struct tree_phi_node phi_node;
  struct tree_iter_pair iter_pair;
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
#define TYPE_FUNCTION_ARGS(node) \
  ((node)->type_node.properties.functional.arg_types)
#define TYPE_FUNCTION_RET(node) \
  ((node)->type_node.properties.functional.ret_types)
#define TYPE_LIST(node) ((node)->type_node.list)
#define TYPE_IS_STREAM(node) ((node)->type_node.is_stream)

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
      if (TREE_CODE_CLASS (code) == tcl_statement)
	return node->stmt_node.operands[idx];
      else if (code == CIRCUMFLEX)
	return node->circumflex_op_node.operands[idx];
      else if (code == ITER_PAIR)
	return node->iter_pair.operands[idx];
      else if (code == FUNCTION)
	return node->function_node.operands[idx];
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
      if (TREE_CODE_CLASS (code) == tcl_statement)
	node->stmt_node.operands[idx] = value;
      else if (code == CIRCUMFLEX)
	node->circumflex_op_node.operands[idx] = value;
      else if (code == ITER_PAIR)
	node->iter_pair.operands[idx] = value;
      else if (code == FUNCTION)
	node->function_node.operands[idx] = value;
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
#define TREE_ID_SOURCE_NAME(node) ((node)->identifier_node.source_name)
#define TREE_ID_DEFINED(node) ((node)->identifier_node.defined)
#define TREE_ID_WITH_PREFIX(node) ((node)->identifier_node.with_prefix)
#define TREE_ID_DEF(node) ((node)->identifier_node.def)
#define TREE_ID_ITER(node) ((node)->identifier_node.iter_desc)
#define TREE_ID_ITER_DEF(node) ((node)->identifier_node.iter_def)
#define TREE_ID_UD_CHAIN(node) ((node)->identifier_node.ud_chain)
#define TREE_ID_DU_CHAIN(node) ((node)->identifier_node.du_chain)
#define is_iter_cases(node) (TREE_CODE (TREE_OPERAND ((node), 1)) == LIST)

#define TREE_STMT_IS_REDUNDANT(node) ((node)->stmt_node.is_redundant)
#define TREE_STMT_REDUNDANCE_CHECKED(node) \
			      ((node)->stmt_node.redundance_checked)
#define TREE_STMT_PARENT_IF(node) ((node)->stmt_node.parent_if)
#define TREE_STMT_DEF_NUMBER(node) ((node)->stmt_node.def_number)
#define TREE_STMT_IS_RECURRENCE_DEF(node) ((node)->stmt_node.is_recurrence_def)
#define TREE_STMT_DEFS(node) ((node)->stmt_node.defs)
#define TREE_STMT_USES(node) ((node)->stmt_node.uses)

#define TREE_ITER_PAIR_LOWER(node) ((node)->iter_pair.lower_index)

#define TREE_ITER_MIN(node) ((node)->rec_expr_node.min_value)
#define TREE_ITER_SIZE(node) ((node)->rec_expr_node.size)
#define TREE_ITER_LIST(node) ((node)->rec_expr_node.list)

#define TREE_FUNC(node) ((node)->function_node)
#define TREE_FUNC_BB_VARS(node) ((node)->function_node.bb_vars)
#define TREE_FUNC_CFG(node) ((node)->function_node.cfg)
#define TREE_FUNC_ENTRY(node) ((node)->function_node.entry_stmts)
#define TREE_FUNC_RETURN(node) ((node)->function_node.return_stmts)
#define TREE_FUNC_PRINT(node) ((node)->function_node.print_stmts)
#define TREE_FUNC_SCHEDULE(node) ((node)->function_node.schedule)
#define TREE_FUNC_NAME(node) ((node)->function_node.operands[0])
#define TREE_FUNC_ARGS(node) ((node)->function_node.operands[1])
#define TREE_FUNC_ARG_TYPES(node) ((node)->function_node.operands[2])
#define TREE_FUNC_RET_TYPE(node) ((node)->function_node.operands[3])
#define TREE_FUNC_INSTRS(node) ((node)->function_node.operands[4])

#define TREE_IS_FUNCTION_PROTO (TREE_FUNC_INSTRS(node) == NULL)

#define TREE_CIRCUMFLEX_INDEX_STATUS(node) ((node)->circumflex_op_node.is_index)

static inline bool
is_assignment_operator (enum eq_token_kind tk)
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
void free_tree_type (tree, bool);
void free_tree_list (struct tree_list_element *);
void free_atomic_trees (void);
tree make_string_cst_tok (struct eq_token *);
tree make_string_cst_str (const char *);
tree make_identifier_tok (struct eq_token *);
tree make_integer_cst (int);
tree make_integer_tok (struct eq_token *);
tree make_real_tok (struct eq_token *);
tree make_tree_list (void);
bool tree_list_append (tree, tree);
tree make_function (tree, tree, tree, tree, tree, struct eq_location);
tree make_type (enum tree_code);
tree change_stream_prop (tree);
tree make_binary_op (enum tree_code, tree, tree);
tree make_unary_op (enum tree_code, tree, struct eq_location);
tree make_matrix (tree, struct eq_location);
tree make_genar (tree, tree, struct eq_location);
tree make_parallel_loop (tree, tree, tree);
tree make_return (tree, struct eq_location);
tree make_convert (tree, tree);
tree tree_copy (tree);
bool tree_compare (tree, tree);
void tree_list_combine (tree, tree);
void tree_list_split (tree, tree);
void free_list (tree);
tree eliminate_list (tree);
char* tree_to_str (tree);

#endif /* __TREE_H__  */
