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

#include <stdio.h>
#include <stdlib.h>

#include "eq.h"
#include "global.h"
#include "uthash.h"
#include "parser.h"

struct token_list_el
{
  struct token *value;
  struct token_list_el *next;
};

struct tree_list_el
{
  tree value;
  struct tree_list_el *next;
};

/* A hash table for token matches.
   A token string value is used here as a key. This is because we ought to put
   here a pointer to the key. However, to use token class as another key (in
   addition to the value) would be better.
   Here we should be aware of two cases:
   -- Because of the fact token's value can be either pointer or enum and we
      don't validate token class before, comparision between pointer and
      integer can be possible theoretically, however this situation is
      unlikely to happen, as pointer values usually are much bigger than enum
      values.
   -- At this point, all string represantations of tokens have the only token
      class in correspondance. If several token classes can be relevant we
      are to include token class to the key as well.  */
struct match_table
{
  const char *key;
  struct token_list_el *match;
  tree replace;
  UT_hash_handle hh;
};

/* We store all substitution rules in this table  */
struct match_table *matches;

#define MATCHER_MATCH(id) ((id)->match)
#define MATCHER_REPLACE(id) ((id)->replace)
#define MATCHER_KEY(id) ((id)->key)

void matcher_init (void);
void add_match (const char *, struct token_list_el *, tree);
void delete_match (struct match_table *);
struct match_table *find_match (const char *);
tree perform_transform (struct parser *);
bool validate_match (struct token_list_el *, tree);
void matcher_finalize (void);
void print_matches (void);
