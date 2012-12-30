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

#define HASH_FREE(hh,head,el,tmp) \
do \
{ \
  HASH_ITER (hh, head, el, tmp) \
    { \
      HASH_DEL (head, el); \
      free (el); \
    } \
  head = NULL; \
} while (0)

struct tree_hash_node
{
  tree s;
  UT_hash_handle hh;
};

/* We store defined variables in a hash table.
   If static single assignment is off, we are redefining every variable redefinition.  */
struct id_defined
{
  const char *id;	    /* An original name of the redefined variable.  
			       Used as a key in a hash table.  */
  bool was_modified;
  struct tree_hash_node *phi_node;/* Information about definitions.  */
  UT_hash_handle hh;
};

struct id_defined* ssa_copy_var_hash (struct id_defined*);
void ssa_verify_vars (basic_block, tree, tree);

#endif /* __SSA_H__ */
