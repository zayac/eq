/* Copyright (c) 2012 Artem Shinkarov <artyom.shinkaroff@gmail.com>
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

#ifndef __SSA_H__
#define __SSA_H__

#include "global.h"
#include "uthash.h"
#include "tree.h"
#include "controlflow.h"

/* We store defined variables in a hash table.
   If static single assignment is on, we throw errors in case the same variables
   are defined one more time.
   If static single assignment is off, we are redefining every variable redefinition.  */
struct id_defined
{
  const char *id;	   /* An original name of the redefined variable.  
			      Used as a key in a hash table.  */
#ifndef SSA 
  int counter;		   /* A number whose string representation is appended 
			      to a varaible on every redifinition.  Then this 
			      number is incremented.  */
  unsigned counter_length; /* The number of digits in `counter' number. We need
			      this while allocating memory for string 
			      representation.  */
  unsigned divider;	    /* A helper field for fast `counter_length'
			       variable track. divider = 10^counter_length.  */
  char* id_new;		    /* A new name for a redefined variable.  */
#endif
  UT_hash_handle hh;
};

/* An id-tree hash table entry.  */
#ifndef SSA
struct id_defined_tree
{
  char *key;
  tree var;
  UT_hash_handle hh;
};

extern struct id_defined *id_definitions;

struct block_variables 
{
  struct tree_list_element *key;
  UT_array *vars;
  UT_hash_handle hh;
};

void ssa_reassign_var (struct id_defined*,
		       tree, tree, tree);
struct block_variables* ssa_declare_new_block
		       (struct tree_list_element *,
		        tree);
void ssa_create_block_vars (struct tree_list_element *, tree);
void ssa_register_new_var (tree);
tree ssa_create_phi_node (basic_block, tree);
tree ssa_localize_phi_node (basic_block, tree);
#else
void ssa_hash_add_var (tree);
#endif
void ssa_free_id_hash (void);
void ssa_register_var_func_list (tree, tree);

#endif /* __SSA_H__  */
