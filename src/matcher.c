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

#include "expand.h"
#include "global.h"
#include "uthash.h"

struct token_list_el
{
  struct token * value;
  struct token_list_el * next;
};

/* A hash table for token matchs  */
struct match_table
{
  struct token key;
  struct token_list_el* match;
  struct token_list_el* replace;
  UT_hash_handle hh;
};

/* We store all substitution rules in this table  */
struct match_table * matches;

#define MATCHER_MATCH(id) ((id)->match)
#define MATCHER_REPLACE(id) ((id)->replace)
#define MATCHER_KEY(id) ((id)->key)

void matcher_init ()
{
  matches = NULL;
}

/* Adds match rule to the hash table  */
void add_match (struct token key, struct token_list_el* match, 
		    struct token_list_el* replace)
{
  struct match_table * el;
  el = (struct match_table *) malloc (sizeof (struct match_table));
  MATCHER_KEY(el) = key;
  MATCHER_MATCH(el) = match;
  MATCHER_REPLACE(el) = replace;
  HASH_ADD (hh, matches, key, sizeof (struct token), el);

}

/* Removes match rule from the hash table  */
void delete_match (struct match_table * del)
{
  struct token_list_el * el;
  struct token_list_el * tmp;
  HASH_DEL (matches, del);
  LL_FOREACH_SAFE(MATCHER_MATCH(del), el, tmp)
    {
      token_free (el->value);
      free (el);
    }

  LL_FOREACH_SAFE(MATCHER_REPLACE(del), el, tmp)
    {
      token_free (el->value);
      free (el);
    }
  free (del);
}

struct match_table* find_match (struct token * tok)
{
  struct match_table * ret = NULL;
  HASH_FIND(hh, matches, tok, sizeof (struct token), ret);
  return ret;
}

void matcher_finalize ()
{
  struct match_table * current, * tmp;
  HASH_ITER(hh, matches, current, tmp)
    {
      delete_match (current);
    }
}

/* Print rules. For debugging purposes  */
void print_matches ()
{
  struct match_table * current, * tmp;
  struct token_list_el *el;
  HASH_ITER(hh, matches, current, tmp)
    {
      LL_FOREACH(MATCHER_MATCH(current), el)
	{
	  fprintf (stdout, "%s ", token_as_string (el->value));
	}
      fprintf (stdout, " -> ");
      LL_FOREACH(MATCHER_REPLACE(current), el)
	{
	  fprintf (stdout, "%s ", token_as_string (el->value));
	}
      fprintf(stdout, "\n");
    }
}
