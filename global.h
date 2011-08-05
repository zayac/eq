/* Copyright (c) 2011 Artem Shinkarov <artyom.shinkaroff@gmail.com>
  
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

#ifndef __GLOBAL_H__
#define __GLOBAL_H__

#include <stdarg.h>
#include "tree.h"

/* Structure to store a list of user-defined types
   FIXME it should be a hash-table!  */
extern tree type_list;
extern tree constant_list;
extern tree function_list;
extern tree function_proto_list;

extern int error_count;
extern int warning_count;

void init_global ();
void finalize_global ();
void init_global_tree ();
void finalize_global_tree ();
void init_function_protos ();


tree type_defined (const char *  name);
tree add_user_type (tree type);
tree function_exists (const char *);
tree expand_exists (const char *);
tree constant_exists (const char *);
bool type_lists_eq (tree, tree);
tree make_function_proto_args (int, va_list);


#endif /* __GLOBAL_H__  */
