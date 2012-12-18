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
add_match (const char *key, struct token_list_el *match, tree replace)
{
  struct match_table *el;
  el = (struct match_table *) malloc (sizeof (struct match_table));
  MATCHER_KEY (el) = key;
  MATCHER_MATCH (el) = match;
  MATCHER_REPLACE (el) = replace;
  HASH_ADD_KEYPTR (hh, matches, key, strlen (key), el);
}

/* Removes match rule from the hash table  */
static void
delete_match (struct match_table *del)
{
  struct token_list_el *el;
  struct token_list_el *tmp;
  HASH_DEL (matches, del);
  /* Free match list  */
  LL_FOREACH_SAFE (MATCHER_MATCH (del), el, tmp)
  {
    eq_token_free (el->value);
    free (el);
  }

  /* Free replacement tree  */
  free_tree (MATCHER_REPLACE (del));

  free (del);
}

/* Find a relevant record in hash table by key token  */
struct match_table *
find_match (const char *str)
{
  struct match_table *ret = NULL;
  HASH_FIND_STR (matches, str, ret);
  return ret;
}


/* A recursive function returning a "real" tree according
   with it's number from the list if there was an "argset" flag set.
   The old tree is freed.
   In other case there the there is a recursive descent, in the end the
   same tree is returned.  */
static tree
connect_nodes (tree t, const struct tree_list_element * list)
{
  int i;

  if (list == NULL)
    return error_mark_node;

  if (TREE_CODE (t) == LIST)
    {
      struct tree_list_element *el;
      tree tmp;
      DL_FOREACH (TREE_LIST (t), el)
      {
	tmp = el->entry;
	el->entry = connect_nodes (el->entry, list);
	if (el->entry != tmp)
	  free_tree (tmp);
      }
      return t;
    }

  if (TREE_CODE_TYPED (TREE_CODE (t)) && TREE_ARGSET (t))
    {
      int counter;
      for (counter = 1; counter < TREE_ARG (t); counter++)
	list = list->next;
      return tree_copy (list->entry);
    }

  for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE (t)); i++)
    {
      tree op = TREE_OPERAND (t, i);
      TREE_OPERAND_SET (t, i, connect_nodes (op, list));
      if (op != TREE_OPERAND (t, i))
	free_tree (op);
    }
  return t;
}

tree
perform_transform (struct eq_parser *parser)
{
  tree ret = NULL, tmp = NULL;
  struct tree_list_element *tmp_expr = NULL;
  struct tree_list_element *exprs = NULL;
  struct match_table *record = NULL;
  struct eq_token *tok = parser_get_token (parser);

  parser_unget (parser);
  record = find_match (eq_token_as_string (tok));

  if (record != NULL)
    {
      struct token_list_el *el = NULL;
      LL_FOREACH (MATCHER_MATCH (record), el)
      {
	if (eq_token_is_keyword (el->value, tv_expr))
	  {
	    tmp = handle_expr (parser);
	    if (tmp != error_mark_node)
	      {
		tmp_expr =
		  (struct tree_list_element *)
		  malloc (sizeof (struct tree_list_element));
		tmp_expr->entry = tmp;
		tmp_expr->next = NULL;
		LL_APPEND (exprs, tmp_expr);
	      }
	    else
	      {
		struct eq_token *tmp = NULL;
		free_tree_list (exprs);
		/* if PARSER_MATCH_EXPR_ALLOWED flag is set, then
		   we are inside another \match,
		   and we are to skip until right brace.  */
		if (PARSER_MATCH_EXPR_ALLOWED (parser))
		  {
		    parser_unget (parser);
		    if (!eq_token_is_operator
			(parser_get_token (parser), tv_rbrace))
		      parser_get_until_tval (parser, tv_rbrace);
		  }
		else
		  {
		    tmp =
		      parser_get_until_one_of_val (parser, 2, tv_lend,
						   tv_qendif);
		    if (tmp != NULL && eq_token_class (tmp) != tok_eof)
		      parser_unget (parser);
		  }
		return error_mark_node;
	      }
	  }
	else
	  {
	    tok = parser_get_token (parser);
	    if (tok->tok_class == tok_unknown)
	      tok->tok_class = tok_keyword;
	    if (eq_token_compare (tok, el->value))
	      {
		struct eq_token *tmp = NULL;
		free_tree_list (exprs);
		error_loc (eq_token_location (tok),
			   "invalid token for macros `%s` ",
			   eq_token_as_string (tok));
		/* if PARSER_MATCH_EXPR_ALLOWED flag is set, then
		   we are inside another \match,
		   and we are to skip until right brace.  */
		if (PARSER_MATCH_EXPR_ALLOWED (parser))
		  tmp = parser_get_until_tval (parser, tv_rbrace);
		else
		  tmp =
		    parser_get_until_one_of_val (parser, 2, tv_lend,
						 tv_qendif);
		if (eq_token_class (tmp) != tok_eof)
		  parser_unget (parser);
		return error_mark_node;
	      }
	  }
      }
      if (MATCHER_REPLACE (record) != error_mark_node)
	{
	  ret = tree_copy (MATCHER_REPLACE (record));
	  tmp = ret;
	  if (exprs != NULL)
	    ret = connect_nodes (ret, exprs);
	  if (tmp != ret)
	    free_tree (tmp);
	}
      /* Free exprs in the end  */
      free_tree_list (exprs);
    }

  return ret;

}

/* In match we are to validate numbers in \expr { <num> } in the right part
   correspondingly to number of \expr which were enumerated in the left side.
   In case everything is fine return true. Otherwise, return list of trees with
   errors  */
static bool
validate_tree (unsigned expr_number, tree t)
{
  bool ret = true;
  assert (t != NULL, "tree can't be NULL");

  if (TREE_CODE_TYPED (TREE_CODE (t)) && TREE_ARGSET (t)
      && (TREE_ARG (t) < 1 || TREE_ARG (t) > expr_number))
    {
      error_loc (TREE_LOCATION (t), "expression number is invalid");
      ret = false;
    }
  else
    {
      int i;
      for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE (t)); i++)
	{
	  bool is_valid = validate_tree (expr_number, TREE_OPERAND (t, i));
	  if (!is_valid)
	    ret = false;
	}
    }
  return ret;
}

/* Match expression validation should be performed  */
bool
validate_match (struct token_list_el * left, tree right)
{
  struct token_list_el *tmp = NULL;
  unsigned number = 0;
  bool ret = true;
  assert (left != NULL && right != NULL, "arguments can't be NULL");

  if ((!(eq_token_class (left->value) == tok_keyword && left->value->uses_buf))
      && !(eq_is_id (left->value, false)))
    {
      error_loc (eq_token_location (left->value),
		 "a new keyword or id token is allowed here only. `%s` found",
		 eq_token_as_string (left->value));
      ret = false;
    }

  LL_FOREACH (left, tmp)
  {
    if (eq_token_is_keyword (tmp->value, tv_expr))
      number++;
  }
  return validate_tree (number, right) && ret;
}


void
matcher_finalize ()
{
  struct match_table *current, *tmp;
  HASH_ITER (hh, matches, current, tmp)
  {
    delete_match (current);
  }
}

/* Print rules. For debugging purposes  */
void
print_matches ()
{
  struct match_table *current, *tmp;
  struct token_list_el *el;
  xfile *  xf = xfile_init_file_stdout ();

  HASH_ITER (hh, matches, current, tmp)
  {
    fprintf (stdout, "Transformation: \n");
    LL_FOREACH (MATCHER_MATCH (current), el)
    {
      fprintf (stdout, "%s ", eq_token_as_string (el->value));
    }
    fprintf (stdout, " ->     \n");
    print_expression (xf, MATCHER_REPLACE (current));
    fprintf (stdout, "\n");
  }
  xfile_finalize (xf);
}
