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

#include "matcher.h"
#include "parser.h"
#include "print.h"

void 
matcher_init ()
{
  matches = NULL;
}

/* Adds match rule to the hash table  */
void 
add_match (struct token key, struct token_list_el* match, 
		    tree replace)
{
  struct match_table * el;
  el = (struct match_table *) malloc (sizeof (struct match_table));
  MATCHER_KEY(el) = key;
  MATCHER_MATCH(el) = match;
  MATCHER_REPLACE(el) = replace;
  HASH_ADD (hh, matches, key, sizeof (struct token), el);

}

/* Removes match rule from the hash table  */
void 
delete_match (struct match_table * del)
{
  struct token_list_el * el;
  struct token_list_el * tmp;
  HASH_DEL (matches, del);
  /* Free match list  */
  LL_FOREACH_SAFE(MATCHER_MATCH(del), el, tmp)
    {
      token_free (el->value);
      free (el);
    }

  /* Free replacement tree  */
  free_tree (MATCHER_REPLACE (del));
  
  free (del);
}

/* Find a relevant record in hash table by key token  */
struct 
match_table* find_match (struct token * tok)
{
  struct match_table * ret = NULL;
  HASH_FIND(hh, matches, tok, sizeof (struct token), ret);
  return ret;
}

/* A recursive function returning a "real" tree according
   with it's number from the list if there was an "argset" flag set.
   The old tree is freed.
   In other case there the there is a recursive descent, in the end the
   same tree is returned.  */
tree
connect_nodes (tree t, struct tree_list_el * list)
{
  int i;
  if (TREE_CODE_TYPED(TREE_CODE(t)) && TREE_ARGSET(t))
    {
      int counter;
      for (counter = 1; counter < TREE_ARG(t); counter++)
	list = list->next;
      return list->value;
    }

  for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE(t)); i++)
    {
      tree op = TREE_OPERAND (t, i);
      TREE_OPERAND_SET (t, i, connect_nodes(op, list));
      free_tree (op);
    }
  return t;
}

tree
perform_transform (struct parser * parser)
{
  tree ret = NULL, tmp = NULL;
  struct tree_list_el * tmp_expr = NULL;
  struct tree_list_el * exprs = NULL;
  struct match_table * record = NULL;
  struct token * tok = parser_get_token (parser);
  parser_unget (parser);
  tok->loc.col = 0;
  tok->loc.line = 0;
  record = find_match (tok);


  if (record != NULL)
    {
      struct token_list_el * el = NULL;
      unsigned token_count = 0;
      LL_FOREACH(MATCHER_MATCH(record), el)
	{
	  if (token_is_keyword (el->value, tv_expr))
	    {
	      tmp_expr = (struct tree_list_el *) 
		malloc (sizeof (struct tree_list_el));
	      tmp_expr->value = handle_expr (parser);
	      tmp_expr->next = NULL;
	      LL_APPEND (exprs, tmp_expr);
	    }
	  else
	    {
	      tok = parser_get_token (parser);
	      token_count++;
	      if (!token_compare (tok, el->value))
		{
		  while (token_count-- > 0)
		    {
		      parser_unget (parser);
		    }
		  return NULL;
		}
	    }
	}
      ret = tree_copy (MATCHER_REPLACE(record));
      tmp = ret;
      ret = connect_nodes (ret, exprs);
      if (tmp != ret)
	free_tree (ret);

    }
  
  return ret;
}

void 
matcher_finalize ()
{
  struct match_table * current, * tmp;
  HASH_ITER(hh, matches, current, tmp)
    {
      delete_match (current);
    }
}

/* Print rules. For debugging purposes  */
void 
print_matches ()
{
  struct match_table * current, * tmp;
  struct token_list_el *el;
  HASH_ITER(hh, matches, current, tmp)
    {
      fprintf(stdout, "Transformation: \n");
      LL_FOREACH(MATCHER_MATCH(current), el)
	{
	  fprintf (stdout, "%s ", token_as_string (el->value));
	}
      fprintf (stdout, " ->     \n");
      print_expression (stdout, MATCHER_REPLACE(current));
      fprintf(stdout, "\n");
    }
}
