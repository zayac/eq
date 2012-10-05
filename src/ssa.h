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

#include "global.h"
#include "uthash.h"
#include "tree.h"
#include "controlflow.h"

#ifndef SSA 
/* We store defined variables in a hash table.
   If static single assignment is off, we are redefining every variable redefinition.  */
struct id_defined
{
  const char *id;	   /* An original name of the redefined variable.  
			      Used as a key in a hash table.  */
  int counter;		   /* A number whose string representation is appended 
			      to a varaible on every redefinition.  Then this 
			      number is incremented.  */
  unsigned counter_length; /* The number of digits in `counter' number. We need
			      this while allocating memory for string 
			      representation.  */
  unsigned divider;	   /* A helper field for fast `counter_length'
			      variable track. divider = 10^counter_length.  */
  char* id_new;		   /* A new name for a redefined variable.  */

  UT_array *phi_node;	   /* Information about phi node.  */
  UT_hash_handle hh;
};

struct id_defined* ssa_copy_var_hash (struct id_defined*);
void ssa_declare_new_var (basic_block, tree);
char* ssa_reassign_var (basic_block, tree);
void ssa_verify_vars (basic_block, tree);
#endif
