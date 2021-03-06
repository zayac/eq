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

#include <stdio.h>
#include <stdlib.h>
#include <err.h>

#include "tree.h"
#include "global.h"
#include "parser.h"
#include "matcher.h"
#include "types.h"

static struct eq_token *parser_get_token (struct eq_parser *);
static void parser_unget (struct eq_parser *);

static struct eq_token *parser_get_until_one_of_val (struct eq_parser *, int, ...);
static struct eq_token *parser_get_until_tval (struct eq_parser *, enum eq_token_kind);
static struct eq_token *parser_forward_tclass (struct eq_parser *, enum eq_token_class);
static struct eq_token *parser_token_alternative_tval (struct eq_parser *, enum eq_token_kind,
					     enum eq_token_kind);
static bool parser_expect_tval (struct eq_parser *, enum eq_token_kind);
static tree handle_type (struct eq_parser *);
static tree handle_arraytype (struct eq_parser *);
static tree handle_ext_type (struct eq_parser *);
static tree handle_stream_or_ext_type (struct eq_parser *);
static tree handle_list (struct eq_parser *, tree (*)(struct eq_parser *),
		  enum eq_token_kind);
static tree handle_id (struct eq_parser *);
static tree handle_indexes (struct eq_parser *, tree);
static tree handle_idx (struct eq_parser *);
static tree handle_lower (struct eq_parser *);
static tree handle_upper (struct eq_parser *);
static tree handle_function (struct eq_parser *);
static tree handle_linear (struct eq_parser *, tree);
static tree handle_divide (struct eq_parser *);
static tree handle_sexpr (struct eq_parser *);
static tree handle_sexpr_op (struct eq_parser *);
static tree handle_filter (struct eq_parser *);
static tree handle_matrix (struct eq_parser *);
static tree handle_genarray (struct eq_parser *);
static tree handle_expr (struct eq_parser *);
static tree handle_return (struct eq_parser *);
static tree handle_assign (struct eq_parser *, tree);
static tree handle_declare (struct eq_parser *, tree);
static tree handle_instr (struct eq_parser *);
static tree handle_parallel_loop (struct eq_parser *, tree);
static tree handle_parallel_loop_cases (struct eq_parser *);
static tree handle_numx (struct eq_parser *);
static tree handle_idx_numx (struct eq_parser *);
static tree handle_print (struct eq_parser *);
static bool handle_match (struct eq_parser *);
static tree handle_proto (struct eq_parser *);
static tree handle_functiontype (struct eq_parser *);


/* Check if parser is not in any parenthesis/bracket expression.  */
static inline bool
parser_parens_zero (struct eq_parser *parser)
{
  return parser->paren_count == 0 && parser->square_count == 0
	 && parser->brace_count == 0;
}

/* transform string representation of hex number to string representation of
   decimal number.  */
static char *
transform_hex_to_dec (char *hex)
{
  char *ret;
  size_t size = 1;
  long long tmp = 10;
  long num = strtol (hex, NULL, 16);

  while (tmp <= num)
    {
      tmp *= 10;
      size++;
    }
  ret = (char *) malloc (sizeof (char) * (size + 1));
  sprintf (ret, "%li", num);
  free (hex);
  return ret;
}

/* Safely increment or decrement index of token buffer. Make
   sure that negative index equals to size - idx.  */
static inline size_t
buf_idx_inc (const size_t idx, const ssize_t inc, const size_t size)
{
  ssize_t diff = ((ssize_t) idx + inc) % size;
  return diff < 0 ? (size_t)(size - diff) : (size_t)diff;
}

/* Get one token from the lexer or from the token buffer.
   Token is taken from the buffer if parser_unget was
   called earlier. */
static struct eq_token *
parser_get_lexer_token (struct eq_parser *parser)
{
  struct eq_token *tok;

  if (parser->unget_idx == 0)
    {
      /* Skip comments for the time being. We do not skip
	 the comments at the level of lexer, because we
	 can put them in the output program.  */
      while (true)
	{
	  tok = eq_lexer_get_token (parser->lex);
	  if (eq_token_class (tok) != tok_comments
	      && eq_token_class (tok) != tok_whitespace)
	    break;
	  else
	    eq_token_free (tok);
	}

      /* Keep track of brackets.  */
      if (eq_token_class (tok) == tok_operator)
	switch (eq_token_value (tok))
	  {
	  case tv_lparen:
	    parser->paren_count++;
	    break;
	  case tv_rparen:
	    parser->paren_count--;
	    break;
	  case tv_lsquare:
	    parser->square_count++;
	    break;
	  case tv_rsquare:
	    parser->square_count--;
	    break;
	  case tv_lbrace:
	    parser->brace_count++;
	    break;
	  case tv_rbrace:
	    parser->brace_count--;
	    break;
	  default:
	    ;
	  }

      /* If TOKEN_BUFFER is full, we free the token pointed by BUF_START
	 and put the new token on its place, changing BUF_START and
	 BUF_END accordingly.  */
      if ((parser->buf_end + 1) % parser->buf_size == parser->buf_start)
	{
	  eq_token_free (parser->token_buffer[parser->buf_start]);
	  parser->buf_start = (parser->buf_start + 1) % parser->buf_size;
	  parser->token_buffer[parser->buf_end] = tok;
	  parser->buf_end = (parser->buf_end + 1) % parser->buf_size;
	}
      else
	{
	  parser->token_buffer[parser->buf_end] = tok;
	  parser->buf_end = (parser->buf_end + 1) % parser->buf_size;
	}
    }
  else
    {
      ssize_t s;

      /* Return a token from the buffer.  */
      assert (parser->unget_idx < parser->buf_size,
	      "parser buffer holds only up to %i values.", parser->buf_size);

      s = parser->buf_end - parser->unget_idx;
      s = s < 0 ? parser->buf_size + s : (unsigned) s;
      parser->unget_idx--;

      tok = parser->token_buffer[s];
    }

  return tok;
}

/* Move the parser one token back. It means that the consequent
   call of parser_get_token would return the token from buffer,
   not from lexer.  */
static void
parser_unget (struct eq_parser *parser)
{
  parser->unget_idx++;
  assert (parser->unget_idx < parser->buf_size,
	  "parser buffer holds only up to %i values.", parser->buf_size);
}

/* Skip tokens until one of tokens from the list.  */
static struct eq_token *
parser_get_until_one_of_val (struct eq_parser *parser, int number, ...)
{
  struct eq_token *tok;
  int i;

  do
    {
      tok = parser_get_token (parser);
      if (!eq_token_uses_buf (tok))
	{
	  va_list list;
	  va_start (list, number);
	  for (i = 0; i < number; i++)
	    {
	      enum eq_token_kind tkind = va_arg (list, enum eq_token_kind);
	      if (eq_token_value (tok) == tkind)
		return tok;
	    }
	}
    }
  while (eq_token_class (tok) != tok_eof);
  return tok;
}

/* Skip tokens until token with value TKIND would be found.  */
static struct eq_token *
parser_get_until_tval (struct eq_parser *parser, enum eq_token_kind tkind)
{
  struct eq_token *tok;

  do
    {
      tok = parser_get_token (parser);
      if (!eq_token_uses_buf (tok)
	  /* FIXME the following condition makes it impossible
	     to skip until some symbol if you are inside the
	     block or brackets. */
	  /* && parser_parens_zero (parser) */
	  && eq_token_value (tok) == tkind)
	return tok;
    }
  while (eq_token_class (tok) != tok_eof);

  return tok;
}

/* XXX For the time being make it macro in order to see
   the __LINE__ expansions of error_loc.  */
#define parser_forward_tval(parser, tkind)  __extension__	  \
({								  \
  struct eq_token * tok = parser_get_token (parser);		  \
  if (eq_token_uses_buf  (tok)  || eq_token_value (tok) != tkind)	  \
    {								  \
      error_loc (eq_token_location (tok), "unexpected token `%s' ",  \
		 eq_token_as_string (tok));			  \
      tok = NULL;						  \
    }								  \
  tok;								  \
})



/* Get token from lexer. In addition to this,
   it does some hex number processing.  */
static struct eq_token *
parser_get_token (struct eq_parser *parser)
{
  struct eq_token *tok = parser_get_lexer_token (parser);

  /* Check and concatenate \left or \right with delimiters, if necessary  */
  if (eq_token_uses_buf (tok)
      && !(strcmp (eq_token_as_string (tok), "\\left")
	   && strcmp (eq_token_as_string (tok), "\\right")))
    {
      struct eq_token *del = parser_get_token (parser);
      if (!eq_token_is_delimiter (del))
	return del;
      else
	{
	  char *conc = NULL;
	  size_t s = parser->buf_size, e = parser->buf_end;

          if (-1 == asprintf (&conc, "%s%s",
                              eq_token_as_string (tok),
                              eq_token_as_string (del)))
            err (EXIT_FAILURE, "asprintf failed");

          /* Leave one token instead of two  */
	  free (tok->value.cval);
	  tok->value.cval = conc;
	  eq_token_free (parser->token_buffer[buf_idx_inc (e, -1, s)]);
	  parser->buf_end = buf_idx_inc (e, -1, s);
	}
    }
  else if (eq_token_is_keyword (tok, tv_hex))
    {
      size_t s = parser->buf_size, e;
      struct eq_token *ret;

      parser->lex->hex_number = true;
      if (!(tok = parser_forward_tval (parser, tv_lbrace)))
	return tok;

      ret = parser_get_token (parser);
      if (!eq_token_is_number (ret))
	{
	  error_loc (eq_token_location (ret), "unexpected token `%s' ",
		     eq_token_as_string (ret));
	  return ret;
	}

      if (!(tok = parser_forward_tval (parser, tv_rbrace)))
	return tok;
      ret->value.cval = transform_hex_to_dec (ret->value.cval);
      tok = ret;

      /* Substitute 4 tokens which were read
	 with just one in the token buffer  */
      e = parser->buf_end;
      eq_token_free (parser->token_buffer[buf_idx_inc (e, -4, s)]);
      eq_token_free (parser->token_buffer[buf_idx_inc (e, -3, s)]);
      eq_token_free (parser->token_buffer[buf_idx_inc (e, -1, s)]);
      parser->token_buffer[buf_idx_inc (e, -4, s)] =
	parser->token_buffer[buf_idx_inc (e, -2, s)];

      parser->token_buffer[buf_idx_inc (e, -3, s)] = NULL;
      parser->token_buffer[buf_idx_inc (e, -2, s)] = NULL;
      parser->token_buffer[buf_idx_inc (e, -1, s)] = NULL;

      parser->buf_end = buf_idx_inc (e, -3, s);
    }
  return tok;
}


/* Get the next token and check if it's class is what expected.
   Function doesn't unget the token in case the success.
   In case unexpected class print error message.  */
static struct eq_token *
parser_forward_tclass (struct eq_parser *parser, enum eq_token_class tclass)
{
  struct eq_token *tok = parser_get_token (parser);

  if (eq_token_class (tok) != tclass)
    {
      error_loc (eq_token_location (tok), "unexpected token `%s' ",
		 eq_token_as_string (tok));
      return NULL;
    }
  return tok;
}

struct eq_token *
parser_token_alternative_tval (struct eq_parser *parser, enum eq_token_kind first,
			       enum eq_token_kind second)
{
  struct eq_token *tok = parser_get_token (parser);

  if (!eq_token_uses_buf (tok)
      && (eq_token_value (tok) == first || eq_token_value (tok) == second))
    return tok;

  parser_unget (parser);
  return NULL;
}

/* Check if the next token returned by parser_get_token would be
   token with the value TKIND, in case the value is different,
   the error_loc would be called.
   NOTE: function ungets the token after checking it.  */
static bool
parser_expect_tval (struct eq_parser * parser, enum eq_token_kind tkind)
{
  struct eq_token *tok = parser_get_token (parser);
  if (!eq_token_uses_buf (tok) && eq_token_value (tok) == tkind)
    {
      parser_unget (parser);
      return true;
    }
  else
    {
      error_loc (eq_token_location (tok),
		 "token `%s' expected, `%s' token found",
		 eq_token_kind_as_string (tkind), eq_token_as_string (tok));
      parser_unget (parser);
      return false;
    }
}

/* Initialize the parser, allocate memory for token_buffer.  */
bool
eq_parser_init (struct eq_parser * parser, struct eq_lexer * lex)
{
  parser->lex = lex;
  parser->buf_size = 16;
  parser->buf_start = 0;
  parser->buf_end = 0;
  parser->buf_empty = true;
  parser->token_buffer =
    (struct eq_token * *) malloc (parser->buf_size * sizeof (struct eq_token *));
  parser->unget_idx = 0;
  PARSER_MATCH_EXPR_ALLOWED (parser) = false;
  matcher_init ();
  return true;
}

/* Clear the memory allocated for internal structure.
   NOTE: PARSER is not freed.  */
bool
eq_parser_finalize (struct eq_parser * parser)
{
  assert (parser, "attempt to free empty parser");

  matcher_finalize ();
  if (parser->buf_size != 0)
    {
      while (parser->buf_start % parser->buf_size !=
	     parser->buf_end % parser->buf_size)
	{
	  eq_token_free (parser->token_buffer[parser->buf_start]);
	  parser->buf_start = (parser->buf_start + 1) % parser->buf_size;
	}

      if (parser->token_buffer)
	{
	  free (parser->token_buffer);
	}

      eq_lexer_finalize (parser->lex);
    }
  return true;
}

/* Check either token is a valid id.
   `error' indicates either error needs to be shown.  */
bool
eq_is_id (struct eq_token * tok, bool error)
{
  int ret = false;
  if (eq_token_class (tok) == tok_id)
    return true;
  if (eq_token_class (tok) == tok_keyword)
    ret = eq_is_token_id[(int) (eq_token_value (tok) - tv_boolean)];
  if (error && !ret)
    error_loc (eq_token_location (tok), "token `%s' cannot start an id",
	       eq_token_as_string (tok));
  return ret;
}

/*
   comment:
   \comment { <string> }
   In case success returns NULL
 */
static tree
handle_comment (struct eq_parser * parser)
{
  if (!parser_forward_tval (parser, tv_comment))
    return error_mark_node;

  if (!parser_forward_tval (parser, tv_lbrace))
    goto error;

  if (!parser_forward_tclass (parser, tok_string))
    goto error;

  if (!parser_forward_tval (parser, tv_rbrace))
    return error_mark_node;

  return NULL;
error:
  parser_get_until_tval (parser, tv_rbrace);
  return error_mark_node;
}

/* Handle function prototype (declaration). Prototypes are used to instruct
   typechecker not to look for a function definition. In this case, we assume
   that the function is provided by a backend language or it is linked
   dynamically.

   proto:
   \proto { <id> } { [ ext_type ]* } { ext_type } */
static tree
handle_proto (struct eq_parser * parser)
{
  struct eq_token * tok;
  tree name = NULL, arg_types = NULL, ret = NULL;
  struct eq_location loc;

  if (!(tok = parser_forward_tval (parser, tv_proto)))
    return false;

  loc = eq_token_location (tok);

  /* function name.  */
  if (!parser_forward_tval (parser, tv_lbrace))
    {
      parser_get_until_tval (parser, tv_rbrace);
      parser_forward_tval (parser, tv_lbrace);
      parser_get_until_tval (parser, tv_rbrace);
      parser_forward_tval (parser, tv_lbrace);
      parser_get_until_tval (parser, tv_rbrace);
      return false;
    }
  if (!eq_is_id (tok = parser_get_token (parser), true))
    {
      parser_get_until_tval (parser, tv_rbrace);
      parser_forward_tval (parser, tv_lbrace);
      parser_get_until_tval (parser, tv_rbrace);
      return false;
    }
  else
    name = make_identifier_tok (tok);

  if (!parser_forward_tval (parser, tv_rbrace))
    {
      parser_forward_tval (parser, tv_lbrace);
      parser_get_until_tval (parser, tv_rbrace);
      goto error;
    }
  /* argument type list.  */
  if (!parser_forward_tval (parser, tv_lbrace))
    goto error;

  if (!eq_token_is_operator (parser_get_token (parser), tv_rbrace))
    {
      parser_unget (parser);
      arg_types = handle_list (parser, handle_stream_or_ext_type, tv_comma);
      if (arg_types == error_mark_node)
	goto error;
    }
  else
    {
      parser_unget (parser);
      arg_types = NULL;
    }

  if (!parser_forward_tval (parser, tv_rbrace))
    {
      parser_forward_tval (parser, tv_lbrace);
      parser_get_until_tval (parser, tv_rbrace);
      goto error;
    }
  /* return type.  */
  if (!parser_forward_tval (parser, tv_lbrace))
    goto error;

  ret = handle_list (parser, handle_stream_or_ext_type, tv_comma);

  if (ret == NULL || ret == error_mark_node)
    {
      parser_get_until_tval (parser, tv_rbrace);
      goto error;
    }

  if (!parser_forward_tval (parser, tv_rbrace))
    goto error;

  return make_function (name, NULL, arg_types, ret, NULL, loc);

error:
  free_tree (name);
  free_tree (arg_types);
  free_tree (ret);
  return error_mark_node;
}

/*
   This function handles \match expressions, however in this case we don't need
   to build a tree.
   Aware that in the match's "left part" there MUST be a new unique keyword
   straight after the opening brace,
   i.e. \sin, \log, etc. It will be used as a keyword.
   In addition, \left {, \left |, etc. (the full list is mentioned in
   delimiters.def) can be used on this place too.
   Further in the "left part" you can use ANY token sequence. New keywords are
   accepted as well.
   In the "right part" there has to be a valid expression.
   Constructions \expr { <num> } are accepted too
   Returns false in case of an error, otherwise -- true.

   match:
   \match { expr } { expr } <-- Not a proper description, see the comment above
 */
static bool
handle_match (struct eq_parser * parser)
{
  struct eq_token *tok = NULL;
  struct token_list_el *match_head = NULL;
  unsigned braces = 0;
  tree replace = NULL;

  if (!parser_forward_tval (parser, tv_match))
    return false;
  if (!parser_forward_tval (parser, tv_lbrace))
    {
      parser_get_until_tval (parser, tv_rbrace);
      parser_forward_tval (parser, tv_lbrace);
      parser_get_until_tval (parser, tv_rbrace);
      return false;
    }

  /* Check if match with the same key exist  */
  tok = parser_get_token (parser);
  parser_unget (parser);
  if (find_match (eq_token_as_string (tok)))
    {
      error_loc (eq_token_location (tok),
		 "A match with key value `%s' is already defined",
		 eq_token_as_string (tok));
      return false;
    }
  while (true)
    {
      struct token_list_el *el = NULL;
      tok = eq_token_copy (parser_get_token (parser));

      /* We need to end up when there is a right brace encountered,
	 and all inclusive braces are closed  */
      if (eq_token_is_operator (tok, tv_rbrace) && !braces)
	{
	  eq_token_free (tok);
	  break;
	}
      else if (eq_token_value (tok) == tv_eof)
	{
	  eq_token_free (tok);
	  error_loc (eq_token_location (tok), "unexpected end of file");
	  return false;
	}
      else if (eq_token_is_keyword (tok, tv_match))
	{
	  error_loc (eq_token_location (tok),
		     "it's impossible to have nesting \\match");
	  parser_get_until_tval (parser, tv_rbrace);
	  return false;
	}
      else if (eq_token_value (tok) == tv_lbrace)
	braces++;
      else if (eq_token_value (tok) == tv_rbrace)
	braces--;

      el = (struct token_list_el *) malloc (sizeof (struct token_list_el));
      if (eq_token_class (tok) == tok_unknown)
	tok->tok_class = tok_keyword;
      el->value = tok;
      el->next = NULL;
      LL_APPEND (match_head, el);
    }

  if (!parser_forward_tval (parser, tv_lbrace))
    {
      parser_get_until_tval (parser, tv_rbrace);
      return false;
    }

  if (!eq_token_is_operator (parser_get_token (parser), tv_rbrace))
    {
      parser_unget (parser);
      PARSER_MATCH_EXPR_ALLOWED (parser) = true;
      replace = handle_expr (parser);
      PARSER_MATCH_EXPR_ALLOWED (parser) = false;
      if (!parser_forward_tval (parser, tv_rbrace))
	{
	  parser_unget (parser);
	  /* If an error occured, it's not essential to add invalid match rule
	     to the table. However, in this way we can avoid future error
	     messages. To mark this match as invalid we will mark a relevant
	     tree as an error tree.  */
	  if (validate_match (match_head, replace))
	    {
	      if (match_head != NULL)
		add_match (eq_token_as_string (match_head->value), match_head,
			   error_mark_node);
	    }
	  return false;
	}
    }
  else
    parser_unget (parser);

  if (!validate_match (match_head, replace))
    return false;
  if (match_head != NULL)
    add_match (eq_token_as_string (match_head->value), match_head, replace);

  return true;
}

/*
    print:
    \print { expr [ , expr]* }
 */
static tree
handle_print (struct eq_parser * parser)
{
  tree t, ret;
  struct eq_token *tok = parser_get_token (parser);
  struct eq_location loc = eq_token_location (tok);

  if (!eq_token_is_keyword (tok, tv_print))
    {
      parser_unget (parser);
      return error_mark_node;
    }

  if (!parser_forward_tval (parser, tv_lbrace))
    return error_mark_node;

  t = handle_list (parser, handle_expr, tv_comma);
  if (t == error_mark_node)
    return error_mark_node;

  ret = make_tree (PRINT_MARK);
  TREE_OPERAND_SET (ret, 0, t);
  TREE_LOCATION (ret) = loc;
  if (!parser_forward_tval (parser, tv_rbrace))
    {
      free_tree (ret);
      return error_mark_node;
    }
  return ret;
}

/*
   functiontype:
   ( [ ext_type [ , ext_type ]* ] \to  ext_type [ , ext_type ]* )
 */
static tree
handle_functiontype (struct eq_parser *parser)
{
  tree t = NULL, tmp = NULL;
  struct eq_token *tok;
  struct eq_location loc;

  /* memorize expression location.  */
  parser_unget (parser);
  tok = parser_get_token (parser);
  loc = eq_token_location (tok);

  if (!parser_forward_tval (parser, tv_lparen))
    goto error;

  tok = parser_get_token (parser);
  parser_unget (parser);
  if (eq_token_is_keyword (tok, tv_to))
    tmp = make_tree_list ();
  else
    tmp = handle_list (parser, handle_stream_or_ext_type, tv_comma);
  if (tmp == error_mark_node)
    goto error;

  t = make_type (FUNCTION_TYPE);

  TYPE_FUNCTION_ARGS (t) = tmp;

  if (!parser_forward_tval (parser, tv_to))
    goto error;

  tmp = handle_list (parser, handle_stream_or_ext_type, tv_comma);
  if (tmp == error_mark_node)
    goto error;

  TYPE_FUNCTION_RET (t) = tmp;

  if (!parser_forward_tval (parser, tv_rparen))
    goto error;

  TREE_LOCATION (t) = loc;

  tmp = types_assign_type (t);
  if (t != tmp)
    free_tree_type (t, false);
  return tmp;
error:
  free_tree (tmp);
  free_tree (t);
  return error_mark_node;
}

/*
   type:
   \type { (Z | R | N | B) }
 */
static tree
handle_type (struct eq_parser * parser)
{
  tree t, ret;
  struct eq_token *tok;

  if (!parser_forward_tval (parser, tv_type))
    return error_mark_node;

  if (!parser_forward_tval (parser, tv_lbrace))
    goto error;

  tok = parser_get_token (parser);

  if (eq_token_uses_buf (tok))
    goto error;


  if (eq_token_value (tok) == tv_boolean)
    t = make_type (B_TYPE);
  else if (eq_token_value (tok) == tv_natural)
    t = make_type (N_TYPE);
  else if (eq_token_value (tok) == tv_integer)
    t = make_type (Z_TYPE);
  else if (eq_token_value (tok) == tv_real)
    t = make_type (R_TYPE);
  else
    goto error;

  TREE_LOCATION (t) = eq_token_location (tok);

  if (!parser_forward_tval (parser, tv_rbrace))
    return error_mark_node;

  ret = types_assign_type (t);
  if (ret != t)
    free_tree_type (t, true);
  return ret;
error:
  parser_get_until_tval (parser, tv_rbrace);
  return error_mark_node;
}

/*
   We use this function to avoid code repetition in handle_ext_type while
   parsing circumflex (^) part of expression
*/
static inline tree
upper_type_wrapper (struct eq_parser *parser)
{
  struct eq_token *tok = NULL;
  tree dim;

  tok = parser_get_token (parser);
  if (eq_token_class (tok) == tok_intnum)
    dim = make_integer_tok (tok);
  else if (eq_is_id (tok, false))
    dim = make_identifier_tok (tok);
  else if (eq_token_is_operator (tok, tv_lbrace))
    {
      dim = handle_sexpr (parser);
      if (!parser_forward_tval (parser, tv_rbrace))
	return error_mark_node;
    }
  else
    {
      error_loc (eq_token_location (tok), "unexpected token `%s' ",
		 eq_token_as_string (tok));
      return error_mark_node;
    }

  if (dim == NULL || dim == error_mark_node)
    return error_mark_node;
  else
    return dim;
}

static tree
handle_sexpr_or_ldots (struct eq_parser *parser)
{
  struct eq_token *tok = parser_get_token (parser);
  tree t;
  if (eq_token_is_keyword (tok, tv_ldots))
    {
      t = make_tree (LDOTS_EXPR);
      TREE_LOCATION (t) = eq_token_location (tok);
      return t;
    }
  else
    {
      parser_unget (parser);
      return handle_sexpr (parser);
    }
}

/*
   arraytype:
   \arraytype { sexpr | \ldots [ , sexpr | \ldots ]* }{Z|N|R|N}
 */
static tree
handle_arraytype (struct eq_parser * parser)
{
  struct eq_token *tok = parser_get_token (parser);
  tree ret, t = NULL, shape;
  struct tree_list_element *el;
  int dim = 0;
  bool shape_undef = false;
  if (!eq_token_is_keyword (tok, tv_arraytype))
    return error_mark_node;

  if (!parser_forward_tval (parser, tv_lbrace))
    return error_mark_node;

  shape = handle_list (parser, handle_sexpr_or_ldots, tv_comma);

  if (shape == error_mark_node)
    return shape;

  if (!parser_forward_tval (parser, tv_rbrace))
    goto error;

  if (!parser_forward_tval (parser, tv_lbrace))
    goto error;

  tok = parser_get_token (parser);
  if (eq_token_uses_buf (tok))
    goto error;

  if (eq_token_value (tok) == tv_boolean)
    t = make_type (B_TYPE);
  else if (eq_token_value (tok) == tv_natural)
    t = make_type (N_TYPE);
  else if (eq_token_value (tok) == tv_integer)
    t = make_type (Z_TYPE);
  else if (eq_token_value (tok) == tv_real)
    t = make_type (R_TYPE);
  else
    goto error;

  if (!parser_forward_tval (parser, tv_rbrace))
    goto error;

  DL_FOREACH (TREE_LIST (shape), el)
    {
      if ((TREE_CODE (el->entry) == LDOTS_EXPR && !shape_undef && dim > 0)
       || (TREE_CODE (el->entry) != LDOTS_EXPR && shape_undef && dim > 0))
	{
	  error_loc (TREE_LOCATION (t), "at the moment it's possible either to "
			    "fully define the shape or not defining it at all");
	  goto error;
	}
      if (TREE_CODE (el->entry) == LDOTS_EXPR)
	shape_undef = true;
      dim++;
    }


  if (shape_undef)
    {
      free_tree (shape);
      shape = NULL;
    }

  TYPE_DIM (t) = make_integer_cst (dim);
  TYPE_SHAPE (t) = shape;
  ret = types_assign_type (t);
  if (ret != t)
    free_tree_type (t, true);

  return ret;
error:
  free_tree (shape);
  free_tree (t);
  return error_mark_node;
}

/*
  ext_type:
  type | arraytype | functiontype
 */
static tree
handle_ext_type (struct eq_parser * parser)
{
  struct eq_token * tok = parser_get_token (parser);

  if (eq_token_is_keyword (tok, tv_type))
    {
      parser_unget (parser);
      return handle_type (parser);
    }
  else if (eq_token_is_operator (tok, tv_lparen))
    {
      parser_unget (parser);
      return handle_functiontype (parser);
    }
  else if (eq_token_is_keyword (tok, tv_arraytype))
    {
      parser_unget (parser);
      return handle_arraytype (parser);
    }
  else
    return error_mark_node;
}

/*
  stream_or_ext_type:
  \overline { ext_type }
  | ext_type
  */
static tree
handle_stream_or_ext_type (struct eq_parser * parser)
{
  tree t = NULL;

  if (eq_token_is_keyword (parser_get_token (parser), tv_overline))
    {
      if (!parser_forward_tval (parser, tv_lbrace))
	return error_mark_node;

      t = handle_ext_type (parser);
      if (t != error_mark_node)
	t = change_stream_prop (t);

      if (!parser_forward_tval (parser, tv_rbrace))
	return error_mark_node;
    }
  else
    {
      parser_unget (parser);
      return handle_ext_type (parser);
    }

  return t;
}

static tree
handle_id (struct eq_parser * parser)
{
  struct eq_token *tok;
  tree t;

  if (!eq_is_id (tok = parser_get_token (parser), true))
    return error_mark_node;
  else
    t = make_identifier_tok (tok);
  return t;
}

/*
   We use this function to avoid code repetition in handle_idx while parsing
   'upper' production
*/
static inline tree
upper_wrapper (struct eq_parser *parser, tree t)
{
  tree idx;

  if (eq_token_is_operator (parser_get_token (parser), tv_circumflex))
    {
      parser_unget (parser);
      idx = handle_upper (parser);
      if (idx != NULL && idx != error_mark_node)
	TREE_OPERAND_SET (idx, 0, t);
      else
	return error_mark_node;
    }
  else
    {
      parser_unget (parser);
      idx = t;
    }
  return idx;
}

/*
   indexes:
    ( [ upper ] [ lower ] | [ lower ] [ upper ] )
    | ^ linear
*/
static tree
handle_indexes (struct eq_parser * parser, tree prefix)
{
  tree up = NULL, low = NULL;
  struct eq_location up_loc;
  struct eq_token *tok;
  if (prefix == error_mark_node)
    return error_mark_node;

  if (eq_token_is_operator (tok = parser_get_token (parser), tv_circumflex))
    {
      up_loc = eq_token_location (tok);
      if (eq_token_is_operator (parser_get_token (parser), tv_lbrace))
	{
	  if (eq_token_is_operator (parser_get_token (parser), tv_lsquare))
	    {
	      if (eq_token_is_keyword (parser_get_token (parser), tv_iter))
		{
		  parser_unget (parser);
		  parser_unget (parser);
		  parser_unget (parser);
		  up = handle_linear (parser, prefix);
		}
	      else
		{
		  parser_unget (parser);
		  up = make_tree (CIRCUMFLEX);
		  TREE_OPERAND_SET (up, 0, prefix);
		  TREE_OPERAND_SET (up, 1, handle_sexpr (parser));
		  TREE_CIRCUMFLEX_INDEX_STATUS (up) = true;
		  parser_forward_tval (parser, tv_rsquare);
		  parser_forward_tval (parser, tv_rbrace);
		}
	      TREE_LOCATION (up) = up_loc;
	    }
	  else
	    {
	      parser_unget (parser);
	      parser_unget (parser);
	      parser_unget (parser);
	      up = upper_wrapper (parser, low);
	    }
	}
      else
	{
	  parser_unget (parser);
	  parser_unget (parser);
	  up = upper_wrapper (parser, low);
	}
      if (TREE_CODE (up) == ERROR_MARK)
	return error_mark_node;

      if (eq_token_is_operator (parser_get_token (parser), tv_lower_index))
	{
	  tree identifier;
	  parser_unget (parser);
	  low = handle_lower (parser);
	  identifier = TREE_OPERAND (up, 0);
	  TREE_OPERAND_SET (up, 0, low);
	  TREE_OPERAND_SET (low, 0, identifier);
	  TREE_LOCATION (low) = TREE_LOCATION (identifier);
	  return up;
	}
      else
	parser_unget (parser);
    }
  else
    parser_unget (parser);

  if (!up || !TREE_CIRCUMFLEX_INDEX_STATUS (up))
    {
      if (eq_token_is_operator (parser_get_token (parser), tv_lower_index))
	{
	  parser_unget (parser);
	  low = handle_lower (parser);
	  if (low != NULL && low != error_mark_node)
	    {
	      TREE_OPERAND_SET (low, 0, prefix);
	      TREE_LOCATION (low) = TREE_LOCATION (prefix);
	    }
	  else
	    return error_mark_node;
	}
      else
	parser_unget (parser);

      /* In case we not encountered 'upper' production before the 'lower',
	 we have to check now - may be 'upper' is after the 'lower' one */
      if (!up)
	{
	  if (eq_token_is_operator (parser_get_token (parser), tv_circumflex))
	    {
	      if (eq_token_is_operator (parser_get_token (parser), tv_lbrace))
		{
		  if (eq_token_is_operator (parser_get_token (parser), tv_lsquare))
		    {
		      parser_unget (parser);
		      parser_unget (parser);
		      up = handle_linear (parser, prefix);
		    }
		  else
		    {
		      parser_unget (parser);
		      parser_unget (parser);
		      parser_unget (parser);
		      up = upper_wrapper (parser, low);
		    }
		}
	      else
		{
		  parser_unget (parser);
		  parser_unget (parser);
		  up = upper_wrapper (parser, low);
		}
	    }
	  else
	    parser_unget (parser);
	}
      else
	{
	  if (up == error_mark_node)
	    return error_mark_node;
	  TREE_OPERAND_SET (up, 0, low);
	}
      if (low != NULL && up == NULL)
	return low;
      else if (low == NULL && up == NULL)
	return prefix;
      else if (low == NULL && up != NULL)
	TREE_OPERAND_SET (up, 0, prefix);
    }
  return up;
}

/*
   idx:
   <id> indexes
 */
static tree
handle_idx (struct eq_parser * parser)
{
  return handle_indexes (parser, handle_id (parser));
}


static tree
handle_list (struct eq_parser * parser, tree (*handler) (struct eq_parser *),
	     enum eq_token_kind delim)
{
  tree ret;
  tree t;
  t = handler (parser);
  if (t == error_mark_node)
    return t;

  if (!eq_token_is_operator (parser_get_token (parser), delim))
    {
      ret = make_tree_list ();
      tree_list_append (ret, t);
    }
  else
    {
      tree list = make_tree_list ();
      tree_list_append (list, t);
      while (true)
	{
	  t = handler (parser);

	  if (t != NULL && t != error_mark_node)
	    tree_list_append (list, t);
	  if (!eq_token_is_operator (parser_get_token (parser), delim))
	    {
	      parser_unget (parser);
	      return list;
	    }
	}
      ret = list;
    }
  parser_unget (parser);

  return ret;
}

/*
   lower
   _ { expr [ , expr ]* }
   | _ ( <id> | numx )
 */
static tree
handle_lower (struct eq_parser * parser)
{
  tree lower = NULL;
  tree t;
  struct eq_token *tok;

  if (!parser_forward_tval (parser, tv_lower_index))
    {
      parser_get_until_tval (parser, tv_rbrace);
      return error_mark_node;
    }

  tok = parser_get_token (parser);
  if (eq_token_is_operator (tok, tv_lbrace))
    {
      t = handle_list (parser, handle_expr, tv_comma);
      if (t && t != error_mark_node)
	if (!parser_forward_tval (parser, tv_rbrace))
	  /* FIXME Why not parser unget?  */
	  goto error;
    }
  else
    {
      t = make_tree_list ();
      if (eq_token_class (tok) == tok_intnum)
	tree_list_append (t, make_integer_tok (tok));
      else if (eq_is_id (tok, false))
	tree_list_append (t, make_identifier_tok (tok));
      else if (eq_token_is_keyword (tok, tv_frac)
	       || eq_token_is_keyword (tok, tv_dfrac))
	{
	  tree tmp;
	  parser_unget (parser);
	  tmp = handle_divide (parser);
	  if (tmp == NULL || t == error_mark_node)
	    goto error;
	  tree_list_append (t, tmp);
	}
      else
	goto error;
    }

  if (t == NULL || t == error_mark_node)
    goto error_shift;
  else
    lower = make_binary_op (LOWER, NULL, t);

  return lower;

error_shift:
  parser_get_until_tval (parser, tv_rbrace);

error:
  free_tree (t);
  free_tree (lower);
  return error_mark_node;
}

/* FIXME this function looks scary.  */
static bool
is_end (struct eq_parser * parser, enum eq_token_kind tok)
{
  if (eq_token_is_keyword (parser_get_token (parser), tv_end))
    {
      if (eq_token_is_operator (parser_get_token (parser), tv_lbrace))
	{
	  if (eq_token_value (parser_get_token (parser)) == tok)
	    {
	      if (eq_token_is_operator (parser_get_token (parser), tv_rbrace))
		return true;
	      parser_unget (parser);
	    }
	}
      parser_unget (parser);
    }
  parser_unget (parser);
  return false;
}

static tree
handle_instr_list (struct eq_parser * parser)
{
  struct eq_token *tok = NULL;
  tree instrs = make_tree_list ();
  tree t;
  bool parse_error = false;

  while (true)
    {
      struct tree_list_element *el, *tmp;

      tok = parser_get_token (parser);
      parser_unget (parser);

      if (!(eq_is_id (tok, false) || eq_token_is_keyword (tok, tv_return)
	    || eq_token_is_keyword (tok, tv_qif)
	    || eq_token_is_keyword (tok, tv_match)
	    || eq_token_is_keyword (tok, tv_comment)
	    || eq_token_is_keyword (tok, tv_print)))
	break;

      /* end of file check,  */
      if (eq_token_class (tok) == tok_eof)
	{
	  error_loc (eq_token_location (tok), "unexpected end of file");
	  break;
	}

      /* this one allows instructions separated by comma.  */
      t = handle_list (parser, handle_instr, tv_semicolon);

     if (t == error_mark_node)
      {
	free_tree (instrs);
	return error_mark_node;
      }

      assert (TREE_CODE (t) == LIST, "there should be an instruction list");

      DL_FOREACH_SAFE (TREE_LIST (t), el, tmp)
	{
	  /* There is a convention that if instruction was a \match
	     statement, we return NULL.
	     In this case \lend in the end could be omited.  */
	  if (el->next == NULL && el->entry == NULL)
	    {
	      if (!eq_token_is_keyword (parser_get_token (parser), tv_lend))
		parser_unget (parser);
	    }
	  else if (el->next == NULL)
	    {
	      /* In case we handled qendif, the \lend is not required.  Grab
		 the previous token which would be \qendif.  */
	      parser_unget (parser);
	      if (eq_token_is_keyword (parser_get_token (parser), tv_qendif))
		/* \qendif check succeeded, do nothing.  */
		;
	      else if (parser_expect_tval (parser, tv_lend))
		parser_get_token (parser);
	      else
		parse_error = true;
	    }

	  if (el->entry != NULL && !parse_error)
	    tree_list_append(instrs, el->entry);

	  /* In case the error we will delete the whole list.  */
	  if (!parse_error)
	    {
	      DL_DELETE (TREE_LIST (t), el);
	      free (el);
	    }
	}
      free_tree (t);
    }

  if (parse_error)
    {
      free_tree (instrs);
      free_tree (t);
      return error_mark_node;
    }
  else
    return instrs;
}

/*
   function:
   \begin { eqcode } { id }
   { [ idx [ , idx ]* ] }
   { [ ext_type [ , ext_type ]* ] }
   { [ ext_type [ , ext_type ]*] }
   instr_list
   \end { eqcode}
 */
static tree
handle_function (struct eq_parser * parser)
{
  tree name = NULL, args = NULL, arg_types = NULL,
       ret = NULL, instrs = NULL, t = NULL;
  struct eq_token *tok;
  struct eq_location loc;

  /* \begin.  */
  tok = parser_get_token (parser);
  if (!eq_token_is_keyword (tok, tv_begin))
    return NULL;

  /* {eqcode}.  */
  loc = eq_token_location (tok);
  if (!eq_token_is_operator (parser_get_token (parser), tv_lbrace))
    return NULL;

  tok = parser_get_token (parser);
  if (!eq_token_is_keyword (tok, tv_eqcode))
    return NULL;
  if (!eq_token_is_operator (parser_get_token (parser), tv_rbrace))
    return NULL;

  /* { <id> }.  */
  if (!parser_forward_tval (parser, tv_lbrace))
    goto error;

  if (!eq_is_id (tok = parser_get_token (parser), true))
    goto error;
  else
    name = make_identifier_tok (tok);

  if (!parser_forward_tval (parser, tv_rbrace))
    goto error;

  /* argument name list.  */
  if (!parser_forward_tval (parser, tv_lbrace))
    goto error;

  if (!eq_token_is_operator (parser_get_token (parser), tv_rbrace))
    {
      parser_unget (parser);
      args = handle_list (parser, handle_id, tv_comma);
      if (args == error_mark_node)
	goto error;
    }
  else
    {
      parser_unget (parser);
      args = make_tree_list ();
    }
  if (!parser_forward_tval (parser, tv_rbrace))
    goto error;

  /* argument type list.  */
  if (!parser_forward_tval (parser, tv_lbrace))
    goto error;

  if (!eq_token_is_operator (parser_get_token (parser), tv_rbrace))
    {
      parser_unget (parser);
      arg_types = handle_list (parser, handle_stream_or_ext_type, tv_comma);
      if (arg_types == error_mark_node)
	goto error;
    }
  else
    {
      parser_unget (parser);
      arg_types = make_tree_list ();
    }

  if (!parser_forward_tval (parser, tv_rbrace))
    goto error;

  /* return type.  */
  if (!parser_forward_tval (parser, tv_lbrace))
    goto error;

  ret = handle_list (parser, handle_stream_or_ext_type, tv_comma);

  if (ret == NULL || ret == error_mark_node)
    goto error;

  if (!parser_forward_tval (parser, tv_rbrace))
    goto error;

  instrs = handle_instr_list (parser);

  /* Check whether there is an ending function token sequence.  */
  if (parser_forward_tval (parser, tv_end))
    {
      if (parser_forward_tval (parser, tv_lbrace))
	{
	  if (parser_forward_tval (parser, tv_eqcode))
	    {
	      if (!parser_forward_tval (parser, tv_rbrace))
		goto end_error;
	    }
	  else
	    goto end_error;
	}
      else
	goto end_error;
    }
  else
    goto end_error;

  return make_function (name, args, arg_types, ret, instrs, loc);

end_error:
  parser_get_until_tval (parser, tv_begin);
  goto free_trees;
error:
  while (true)
    {
      parser_get_until_tval (parser, tv_end);
      if (eq_token_value (parser_get_token (parser)) != tv_lbrace)
	continue;
      if (eq_token_value (parser_get_token (parser)) != tv_eqcode)
	continue;
      if (eq_token_value (parser_get_token (parser)) != tv_rbrace)
	continue;
      break;
    }
free_trees:
  free_tree (name);
  free_tree (args);
  free_tree (arg_types);
  free_tree (ret);
  free_tree (instrs);
  free_tree (t);
  return error_mark_node;
}

/*
   function_call:
   \call { <id> } { [ expr [, expr ]* ] }
 */
static tree
handle_call (struct eq_parser * parser, enum eq_token_kind type)
{
  tree t = NULL, args = NULL;
  struct eq_token *tok;

  assert (type == tv_call || type == tv_lambda, "unsupported token kind");
  if (!(tok = parser_forward_tval (parser, type)))
    goto error;

  if (!parser_forward_tval (parser, tv_lbrace))
    goto error;

  if (type == tv_call)
    t = make_tree (FUNCTION_CALL);
  TREE_LOCATION (t) = eq_token_location (tok);

  if (!eq_is_id (tok = parser_get_token (parser), true))
    goto error;
  else
    TREE_OPERAND_SET (t, 0, make_identifier_tok (tok));

  if (!parser_forward_tval (parser, tv_rbrace))
    goto error;
  if (!parser_forward_tval (parser, tv_lbrace))
    goto error;

  tok = parser_get_token (parser);
  if (!eq_token_is_operator (tok, tv_rbrace))
    {
      parser_unget (parser);
      args = handle_list (parser, handle_expr, tv_comma);
      if (args != NULL && args != error_mark_node && TREE_LIST (args) != NULL)
	TREE_OPERAND_SET (t, 1, args);
      else
	goto error;
    }
  else
    {
      TREE_OPERAND_SET (t, 1, make_tree_list ());
      return t;
    }

  if (!parser_forward_tval (parser, tv_rbrace))
    {
      free_tree (t);
      free_tree (args);
      return error_mark_node;
    }

  return t;

error:
  parser_get_until_tval (parser, tv_rbrace);
  free_tree (t);
  free_tree (args);
  return error_mark_node;
}

/*
   upper:
   ^ ( <id> | numx )
 */
static tree
handle_upper (struct eq_parser * parser)
{
  tree circumflex;
  tree t = error_mark_node;
  struct eq_token *tok;
  circumflex = make_tree (CIRCUMFLEX);
  TREE_CIRCUMFLEX_INDEX_STATUS (circumflex) = false;

  if (!parser_forward_tval (parser, tv_circumflex))
    {
      parser_get_until_tval (parser, tv_rbrace);
      return error_mark_node;
    }

  tok = parser_get_token (parser);
  if (eq_is_id (tok, false))
    {
      t = make_identifier_tok (tok);
      TREE_OPERAND_SET (circumflex, 1, t);
    }
  else if (eq_token_class (tok) == tok_intnum
	   || eq_token_class (tok) == tok_realnum)
    {
      if (eq_token_class (tok) == tok_intnum)
	t = make_integer_tok (tok);
      else
	t = make_real_tok (tok);
      TREE_OPERAND_SET (circumflex, 1, t);
    }
  else if (eq_token_is_keyword (tok, tv_frac)
	   || eq_token_is_keyword (tok, tv_dfrac))
    {
      t = handle_divide (parser);
      TREE_OPERAND_SET (circumflex, 1, t);
    }
  else if (eq_token_is_operator (tok, tv_lbrace))
    {
      t = handle_expr (parser);

      if (t == NULL || t == error_mark_node)
	goto error_shift;
      else
	TREE_OPERAND_SET (circumflex, 1, t);

      if (!parser_forward_tval (parser, tv_rbrace))
	{
	  parser_unget (parser);
	  goto error;
	}
    }
  else
    {
      error_loc (eq_token_location (tok), "unexpected token `%s' ",
		 eq_token_as_string (tok));
      goto error;
    }

  return circumflex;

error_shift:
  parser_get_until_tval (parser, tv_rbrace);

error:
  free_tree (t);
  free_tree (circumflex);
  return error_mark_node;
}

/* linear:
   { [ ( \iter [ - <num> ] ) | ( <num> ) ] }
 */
static tree
handle_linear (struct eq_parser * parser, tree prefix)
{
  struct eq_token *tok;
  struct eq_location loc;
  tree t = NULL, circumflex = make_tree (CIRCUMFLEX);

  TREE_CIRCUMFLEX_INDEX_STATUS (circumflex) = true;
  TREE_OPERAND_SET (circumflex, 0, prefix);
  TREE_OPERAND_SET (circumflex, 1, NULL);

  if (!parser_forward_tval (parser, tv_lbrace))
    goto error;

  if (!parser_forward_tval (parser, tv_lsquare))
    goto error;

  if (eq_is_id (tok = parser_get_token (parser), false))
    {
      tree id;
      loc = eq_token_location (tok);
      if (eq_token_class (tok) != tok_keyword
	  || eq_token_value (tok) != tv_iter)
	{
	  error_loc (eq_token_location (tok), "only `%s' identifier can be "
		     "occured in recurrent expression, `%s' found",
		     eq_token_kind_name[tv_iter],
		     eq_token_as_string (tok));

	  goto error;
	}
      id = make_identifier_tok (tok);
      tok = parser_get_token (parser);
      if (eq_token_is_operator (tok, tv_minus))
	{
	  tok = parser_get_token (parser);
	  if (!eq_token_is_number (tok))
	    {
	      parser_unget (parser);
	      parser_unget (parser);
	      t = id;
	    }
	  else
	    {
	      t = make_binary_op (MINUS_EXPR, id, make_integer_tok (tok));
	      /* 'iter_var_node' identifier has location {0, 0} as it is stored
		 globally. Therefore we are to assign location separately for
		 it's parent node.  */
	      if (id == iter_var_node)
		TREE_LOCATION (t) = loc;
	    }
	}
      else
	{
	  parser_unget (parser);
	  TREE_OPERAND_SET (circumflex, 1, id);
	}
    }
  else if (eq_token_is_number (tok))
    t = make_integer_tok (tok);
  else
    {
      error_loc (eq_token_location (tok), "unexpected token `%s'in the recurrent "
		 "expression", eq_token_as_string (tok));
      goto error;
    }

  if (!parser_forward_tval (parser, tv_rsquare))
    goto error;

  if (!parser_forward_tval (parser, tv_rbrace))
    return error_mark_node;

  if (TREE_OPERAND (circumflex, 1) == NULL)
    TREE_OPERAND_SET (circumflex, 1, t);
  return circumflex;
error:
  parser_get_until_tval (parser, tv_rbrace);
  return error_mark_node;
}

/*
   ( \frac | \dfrac) { expr } { expr }
 */
static tree
handle_divide (struct eq_parser * parser)
{
  tree t = NULL, l = NULL, r = NULL;

  struct eq_token *tok =
    parser_token_alternative_tval (parser, tv_frac, tv_dfrac);

  if (tok == NULL)
    {
      parser_unget (parser);
      tok = parser_get_token (parser);
      error_loc (eq_token_location (tok), "unexpected token `%s' ",
		 eq_token_as_string (tok));
      goto error_shift_one;
    }

  if (!parser_forward_tval (parser, tv_lbrace))
    goto error_shift_one;

  l = handle_expr (parser);

  if (l == NULL || l == error_mark_node)
    goto error_shift_one;

  if (!parser_forward_tval (parser, tv_rbrace))
    goto error_shift_two;

  if (!parser_forward_tval (parser, tv_lbrace))
    goto error_shift_two;

  r = handle_expr (parser);
  if (r == NULL || r == error_mark_node)
    goto error_shift_two;

  t = make_binary_op (DIV_EXPR, l, r);

  if (!parser_forward_tval (parser, tv_rbrace))
    goto error;

  return t;
error_shift_one:
  parser_get_until_tval (parser, tv_rbrace);
error_shift_two:
  parser_get_until_tval (parser, tv_rbrace);
error:
  free_tree (l);
  free_tree (r);
  return error_mark_node;
}

/* relations:
 * expr rel expr [ rel expr ]
 */
static tree
handle_relations (struct eq_parser * parser)
{
  tree t1;
  tree t2 = error_mark_node;
  tree t3 = error_mark_node;
  tree rel1 = error_mark_node;
  tree rel2 = error_mark_node;
  struct eq_token *tok;

  t1 = handle_expr (parser);
  if (t1 == NULL || t1 == error_mark_node)
    goto error;

  tok = parser_get_token (parser);

  /* FIXME The code duplication is happening here.  */
  if (eq_token_class (tok) == tok_operator)
    {
      switch (eq_token_value (tok))
	{
	case tv_lt:
	  rel1 = make_binary_op (LT_EXPR, t1, NULL);
	  break;
	case tv_gt:
	  rel1 = make_binary_op (GT_EXPR, t1, NULL);
	  break;
	case tv_geq:
	  rel1 = make_binary_op (GE_EXPR, t1, NULL);
	  break;
	case tv_leq:
	  rel1 = make_binary_op (LE_EXPR, t1, NULL);
	  break;
	case tv_not:
	  if (!parser_forward_tval (parser, tv_eq))
	    goto error;
	  rel1 = make_binary_op (NE_EXPR, t1, NULL);
	  break;
	case tv_neq:
	  rel1 = make_binary_op (NE_EXPR, t1, NULL);
	  break;
	case tv_eq:
	  rel1 = make_binary_op (EQ_EXPR, t1, NULL);
	  break;
	default:
	  error_loc (eq_token_location (tok),
		     "relation operator expected here, `%s' found",
		     eq_token_as_string (tok));
	  parser_unget (parser);
	  goto error;
	}
    }
  else
    {
      error_loc (eq_token_location (tok),
		 "relation operator expected here, `%s' found",
		 eq_token_as_string (tok));
      goto error;
    }

  t2 = handle_expr (parser);
  if (t2 == NULL || t2 == error_mark_node)
    goto error;

  TREE_OPERAND_SET (rel1, 1, t2);

  tok = parser_get_token (parser);

  /* FIXME this code is duplicated.  */
  if (eq_token_class (tok) == tok_operator)
    {
      switch (eq_token_value (tok))
	{
	case tv_lt:
	  rel2 = make_binary_op (LT_EXPR, t2, NULL);
	  break;
	case tv_gt:
	  rel2 = make_binary_op (GT_EXPR, t2, NULL);
	  break;
	case tv_geq:
	  rel2 = make_binary_op (GE_EXPR, t2, NULL);
	  break;
	case tv_leq:
	  rel2 = make_binary_op (LE_EXPR, t2, NULL);
	  break;
	case tv_not:
	  if (!parser_forward_tval (parser, tv_eq))
	    goto error;
	  rel2 = make_binary_op (NE_EXPR, t2, NULL);
	  break;
	case tv_neq:
	  rel1 = make_binary_op (NE_EXPR, t1, NULL);
	  break;
	case tv_eq:
	  rel2 = make_binary_op (EQ_EXPR, t2, NULL);
	  break;
	default:
	  parser_unget (parser);
	  return rel1;
	}
    }
  else
    {
      parser_unget (parser);
      return rel1;
    }

  t3 = handle_expr (parser);

  if (t3 == NULL || t3 == error_mark_node)
    goto error;

  TREE_OPERAND_SET (rel2, 1, t3);
  return make_binary_op (LAND_EXPR, rel1, rel2);

error:
  free_tree (t1);
  free_tree (t2);
  free_tree (t3);
  free_tree (rel1);
  free_tree (rel2);
  return error_mark_node;
}

static tree
handle_cond_block (struct eq_parser * parser)
{
  tree t;
  struct eq_token *tok;
  enum prec
  {
    prec_none,
    prec_logor,
    prec_logand,
    num_precs
  };

  struct
  {
    tree expr;
    enum prec prec;
    enum tree_code op;
  } stack[num_precs];

  int sp = 0;

  if (eq_token_is_operator (parser_get_token (parser), tv_lparen))
    {
      t = handle_cond_block (parser);
      parser_expect_tval (parser, tv_rparen);
      parser_get_token (parser);
    }
  else
    {
      parser_unget (parser);
      t = handle_relations (parser);
    }

  if (t == NULL || t == error_mark_node)
    return error_mark_node;

  stack[0].expr = t;
  stack[0].prec = prec_none;

  while (true)
    {
      enum prec oprec;
      enum tree_code ocode;

      tok = parser_get_token (parser);

      if (eq_token_class (tok) == tok_operator)
	{
	  switch (eq_token_value (tok))
	    {
	    case tv_cap:
	      oprec = prec_logand;
	      ocode = LAND_EXPR;
	      break;
	    case tv_cup:
	      oprec = prec_logor;
	      ocode = LOR_EXPR;
	      break;
	    default:
	      parser_unget (parser);
	      goto out;
	    }

	  while (oprec <= stack[sp].prec)
	    {
	      stack[sp - 1].expr =
		make_binary_op (stack[sp].op, stack[sp - 1].expr,
				stack[sp].expr);
	      sp--;
	    }

	  if (eq_token_is_operator (parser_get_token (parser), tv_lparen))
	    {
	      t = handle_cond_block (parser);
	      parser_expect_tval (parser, tv_rparen);
	      parser_get_token (parser);
	    }
	  else
	    {
	      parser_unget (parser);
	      t = handle_relations (parser);
	    }

	  if (t == NULL || t == error_mark_node)
	    while (sp >= 0)
	      {
		free_tree (stack[sp--].expr);
		return error_mark_node;
	      }

	  sp++;
	  stack[sp].expr = t;
	  stack[sp].prec = oprec;
	  stack[sp].op = ocode;
	}
      else
	{
	  parser_unget (parser);
	  break;
	}
    }

out:
  while (sp > 0)
    {
      stack[sp - 1].expr =
	make_binary_op (stack[sp].op, stack[sp - 1].expr, stack[sp].expr);
      sp--;
    }
  return stack[0].expr;
}

/* expr_match:
   \expr { <num> }
 */
static tree
handle_expr_match (struct eq_parser * parser)
{
  tree t;
  tree tmp;
  struct eq_token *tok = NULL;
  struct eq_location loc;

  if (!(tok = parser_forward_tval (parser, tv_expr)))
    return error_mark_node;

  loc = eq_token_location (tok);

  if (!parser_forward_tval (parser, tv_lbrace))
    {
      parser_unget (parser);
      if (!eq_token_is_operator (parser_get_token (parser), tv_rbrace))
	parser_get_until_tval (parser, tv_rbrace);
      return error_mark_node;
    }

  tok = parser_get_token (parser);

  if (!eq_token_is_number (tok))
    {
      error_loc (eq_token_location (tok), "unexpected token `%s' ",
		 eq_token_as_string (tok));
      parser_get_until_tval (parser, tv_rbrace);
      return error_mark_node;
    }

  if (!parser_forward_tval (parser, tv_rbrace))
    {
      return error_mark_node;
    }

  t = make_tree (EXPR_MATCH);
  TREE_LOCATION (t) = loc;

  TREE_ARGSET (t) = true;
  tmp = make_integer_tok (tok);
  TREE_ARG (t) = tmp->int_cst_node.value;
  free_tree (tmp);
  return t;
}

/* sexpr:
       { expr }
     | ( expr )
     | expr_match   <-- possible only when PARSER_MATCH_EXPR_ALLOWED is set
     |
     sexpr_op [ ( \land | \lor | \oplus | + | -
		| \cdot | divide | \ll | \gg
		| \mod ) expr_op ]*

*/
static tree
handle_sexpr (struct eq_parser * parser)
{
  tree t;
  struct eq_token *tok;
  enum prec
  {
    prec_none,
    prec_logor,
    prec_logand,
    prec_eq,
    prec_rel,
    prec_shift,
    prec_add,
    prec_mult,
    num_precs
  };

  struct
  {
    tree expr;
    enum prec prec;
    enum tree_code op;
  } stack[num_precs];

  int sp = 0;

  tok = parser_get_token (parser);
  if (eq_token_is_operator (tok, tv_lbrace))
    {
      t = handle_expr (parser);
      parser_expect_tval (parser, tv_rbrace);
      parser_get_token (parser);
    }
  else if (eq_token_is_operator (tok, tv_lparen))
    {
      t = handle_expr (parser);
      parser_expect_tval (parser, tv_rparen);
      parser_get_token (parser);
    }
  else if (eq_token_is_keyword (tok, tv_expr))
    {
      if (PARSER_MATCH_EXPR_ALLOWED (parser))
	{
	  parser_unget (parser);
	  return handle_expr_match (parser);
	}
      else
	{
	  error_loc (eq_token_location (tok),
		     "\\expr is allowed only inside \\match rule");
	  return error_mark_node;
	}
    }
  else
    {
      parser_unget (parser);
      t = handle_sexpr_op (parser);
    }

  if (t == NULL || t == error_mark_node)
    return error_mark_node;

  stack[0].expr = t;
  stack[0].prec = prec_none;

  while (true)
    {
      enum prec oprec;
      enum tree_code ocode;

      tok = parser_get_token (parser);

      if (eq_token_class (tok) == tok_operator)
	{
	  switch (eq_token_value (tok))
	    {
	    case tv_cdot:
	      oprec = prec_mult;
	      ocode = MULT_EXPR;
	      break;
	    case tv_mod:
	      oprec = prec_mult;
	      ocode = MOD_EXPR;
	      break;
	    case tv_plus:
	      oprec = prec_add;
	      ocode = PLUS_EXPR;
	      break;
	    case tv_minus:
	      oprec = prec_add;
	      ocode = MINUS_EXPR;
	      break;
	    case tv_ll:
	      oprec = prec_shift;
	      ocode = SLEFT_EXPR;
	      break;
	    case tv_gg:
	      oprec = prec_shift;
	      ocode = SRIGHT_EXPR;
	      break;
	    case tv_land:
	      oprec = prec_rel;
	      ocode = BAND_EXPR;
	      break;
	    case tv_lor:
	      oprec = prec_rel;
	      ocode = BOR_EXPR;
	      break;
	    case tv_oplus:
	      oprec = prec_rel;
	      ocode = XOR_EXPR;
	      break;
	    default:
	      parser_unget (parser);
	      goto out;
	    }

	  while (oprec <= stack[sp].prec)
	    {
	      stack[sp - 1].expr =
		make_binary_op (stack[sp].op, stack[sp - 1].expr,
				stack[sp].expr);
	      sp--;
	    }

	  tok = parser_get_token (parser);
	  if (eq_token_is_operator (tok, tv_lparen))
	    {
	      t = handle_expr (parser);
	      parser_expect_tval (parser, tv_rparen);
	      parser_get_token (parser);
	    }
	  else if (eq_token_is_operator (tok, tv_lbrace))
	    {
	      t = handle_expr (parser);
	      parser_expect_tval (parser, tv_rbrace);
	      parser_get_token (parser);
	    }
	  else
	    {
	      parser_unget (parser);
	      t = handle_sexpr_op (parser);
	    }

	  if (t == NULL || t == error_mark_node)
	    while (sp >= 0)
	      {
		free_tree (stack[sp--].expr);
		return error_mark_node;
	      }

	  sp++;
	  stack[sp].expr = t;
	  stack[sp].prec = oprec;
	  stack[sp].op = ocode;
	}
      else
	{
	  parser_unget (parser);
	  break;
	}
    }

out:
  while (sp > 0)
    {
      stack[sp - 1].expr =
	make_binary_op (stack[sp].op, stack[sp - 1].expr, stack[sp].expr);
      sp--;
    }
  return stack[0].expr;
}

/* sexpr_op:
   [ (\lnot | - ) ] ( idx_numx | function_call
		      | matrix | { sexpr } | ( sexpr ) )
 */
static tree
handle_sexpr_op (struct eq_parser * parser)
{
  tree t = error_mark_node;
  tree t1 = error_mark_node;

  struct eq_token *tok = parser_get_token (parser);

  bool prefix = false;

  if (eq_token_is_operator (tok, tv_minus))
    {
      t = make_unary_op (UMINUS_EXPR, NULL, eq_token_location (tok));
      prefix = true;
    }
  else if (eq_token_is_operator (tok, tv_lnot))
    {
      t = make_unary_op (NOT_EXPR, NULL, eq_token_location (tok));
      prefix = true;
    }

  if (prefix)
    tok = parser_get_token (parser);


  if (eq_token_value (tok) == tv_call
	|| eq_token_value (tok) == tv_lambda)
    {
      if (eq_token_is_operator (parser_get_token (parser), tv_lbrace))
	{
	  parser_unget (parser);
	  parser_unget (parser);
	  t1 = handle_call (parser, eq_token_value (tok));
	}
      else
	{
	  parser_unget (parser);
	  parser_unget (parser);
	  t1 = perform_transform (parser);
	  if (t1 == NULL)
	    t1 = handle_idx_numx (parser);
	}
    }
  else if (eq_token_is_number (tok) || eq_is_id (tok, false)
      || eq_token_is_keyword (tok, tv_frac) || eq_token_is_keyword (tok, tv_dfrac))
    {
      parser_unget (parser);
      t1 = perform_transform (parser);
      if (t1 == NULL)
	t1 = handle_idx_numx (parser);
    }
  else if (eq_token_is_operator (tok, tv_lbrace)
	   || eq_token_is_operator (tok, tv_lparen))
    {
      parser_unget (parser);
      t1 = handle_sexpr (parser);
    }
  else if (eq_token_is_keyword (tok, tv_begin))
    {
      if (eq_token_is_operator (parser_get_token (parser), tv_lbrace))
	{
	  if (eq_token_is_keyword (tok = parser_get_token (parser), tv_tmatrix))
	    {
	      parser_unget (parser);
	      parser_unget (parser);
	      parser_unget (parser);
	      t1 = handle_matrix (parser);
	    }
	  else
	    parser_unget (parser);
	}
      else
	parser_unget (parser);
    }
  else
    {
      /* Try to perform transformations which are recorded in matcher table  */
      parser_unget (parser);
      t1 = perform_transform (parser);
      if (t1 == NULL)
	{
	  error_loc (eq_token_location(tok), "unexpected token `%s' found",
		     eq_token_as_string (tok));
	  return error_mark_node;
	}
    }

  if (prefix)
    {
      if (t1 != error_mark_node)
	TREE_OPERAND_SET (t, 0, t1);
      else
	goto error;
    }
  else
    {
      if (t1 != error_mark_node)
	t = t1;
      else
	goto error;
    }
  return t;

error:
  free_tree (t1);
  free_tree (t);
  return error_mark_node;
}

/*
   if_cond:
   \qif { cond_block } instr_list
   [ \qelseif { cond_block } instr_list] *
   [ \qelse instr_list ] * \qendif
*/
static tree
handle_if_cond (struct eq_parser * parser)
{
  struct eq_token *tok;
  tree iftree = error_mark_node;
  tree cond = error_mark_node;
  tree head = error_mark_node;
  tree t = error_mark_node;
  tree instrs;
  enum first_token
  { IF, ELSEIF, ELSE };
  enum first_token status = IF;

  while (true)
    {
      struct eq_location loc;
      tok = parser_get_token (parser);
      loc = tok->loc;
      if (status == IF)
	{
	  if (!eq_token_is_keyword (tok, tv_qif))
	    {
	      error_loc (loc, "token \\qif expected here, `%s' found",
			 eq_token_as_string (tok));
	      parser_get_until_tval (parser, tv_qendif);
	      goto error;
	    }
	}
      else if (status == ELSEIF)
	{
	  if (!eq_token_is_keyword (tok, tv_qelseif))
	    {
	      error_loc (loc, "token \\qelseif expected here, `%s' found",
			 eq_token_as_string (tok));
	      parser_get_until_tval (parser, tv_qendif);
	      goto error;
	    }
	}
      else
	{
	  if (!eq_token_is_keyword (tok, tv_qelse))
	    {
	      error_loc (loc, "token \\qelse expected here, `%s' found",
			 eq_token_as_string (tok));
	      parser_get_until_tval (parser, tv_qendif);
	      goto error;
	    }
	}

      if (status != ELSE)
	{
	  if (!parser_forward_tval (parser, tv_lbrace))
	    {
	      parser_get_until_tval (parser, tv_qendif);
	      goto error;
	    }

	  cond = handle_cond_block (parser);

	  if (cond == error_mark_node)
	    {
	      parser_get_until_tval (parser, tv_qendif);
	      goto error;
	    }
	  else
	    {
	      if (!parser_forward_tval (parser, tv_rbrace))
		{
		  struct eq_token *tok;
		  tok =
		    parser_get_until_one_of_val (parser, 2, tv_rbrace,
						 tv_qendif);
		  if (eq_token_value (tok) == tv_qendif)
		    goto error;
		}
	    }
	}
      else
	cond = NULL;

      instrs = handle_instr_list (parser);

      if (status == IF || status == ELSEIF)
	{
	  t = make_tree (IF_STMT);
	  TREE_OPERAND_SET (t, 0, cond);
	  TREE_OPERAND_SET (t, 1, instrs);
	  TREE_OPERAND_SET (t, 2, NULL);
	  TREE_LOCATION (t) = loc;
	}


      if (status == IF)
	head = iftree = t;
      else if (status == ELSEIF)
	{
	  TREE_OPERAND_SET (iftree, 2, make_tree_list ());
	  tree_list_append (TREE_OPERAND (iftree, 2), t);
	  iftree = t;
	}
      else
	TREE_OPERAND_SET (iftree, 2, instrs);

      tok = parser_get_token (parser);

      if (eq_token_is_keyword (tok, tv_qelseif))
	{
	  if (status == IF)
	    status = ELSEIF;
	  parser_unget (parser);
	}
      else if (eq_token_is_keyword (tok, tv_qelse))
	{
	  if (status == IF)
	    status = ELSE;
	  if (status == ELSEIF)
	    status = ELSE;
	  parser_unget (parser);
	}
      else if (eq_token_is_keyword (tok, tv_qendif))
	break;
      else
	{
	  error_loc (eq_token_location (tok),
		     "unexpected token `%s' found in if statement",
		     eq_token_as_string (tok));
	  parser_unget (parser);
	  goto error;
	}
    }
  return head;
error:
  free_tree (iftree);
  free_tree (cond);
  free_tree (head);
  free_tree (t);
  return error_mark_node;
}

/*
   generator:
   \forall <id> |
   <id> [ , <id> ]* : sexpr [ comp sexpr ]+ [set_op sexpr [ comp sexpr ]+ ]*
 */
static tree
handle_generator (struct eq_parser * parser)
{
  struct eq_token *tok = parser_get_token (parser);
  tree t;
  tree t1 = error_mark_node;
  tree ret = error_mark_node;

  if (eq_token_is_keyword (tok, tv_forall))
    {
      t = handle_list (parser, handle_id, tv_comma);
      if (t == error_mark_node)
	goto shift;
      else
	{
	  ret = make_tree (FORALL);
	  TREE_OPERAND_SET (ret, 0, t);
	  return ret;
	}
    }
  else
    {
      parser_unget (parser);
      t = handle_list (parser, handle_id, tv_comma);

      if (t != error_mark_node)
	ret = make_binary_op (GENERATOR, t, NULL);
      else
	goto shift;

      if (!parser_forward_tval (parser, tv_colon))
	goto error;

      t1 = handle_cond_block (parser);

      if (t1 == NULL || t1 == error_mark_node)
	goto error;

      TREE_OPERAND_SET (ret, 1, t1);
      return ret;
    }
shift:
  parser_get_until_tval (parser, tv_colon);
error:
  free_tree (t);
  free_tree (t1);
  free_tree (ret);
  return error_mark_node;
}

/*
   filter:
   \filter { <id> | sexpr [ comp sexpr ]+ [set_op sexpr [ comp sexpr ]+ ]* }
*/
static tree
handle_filter (struct eq_parser * parser)
{
  tree ids = NULL, cond = NULL, ret = NULL;

  if (!(parser_forward_tval (parser, tv_filter)))
    goto error;

  if (!parser_forward_tval (parser, tv_lbrace))
    goto error;
  else
    ids = handle_list (parser, handle_id, tv_comma);

  if (!parser_forward_tval (parser, tv_vertical))
    goto error;

  cond = handle_cond_block (parser);

  if (cond == error_mark_node)
    goto error;

  if (!parser_forward_tval (parser, tv_rbrace))
    {
      parser_unget (parser);
      goto error;
    }

  ret = make_binary_op (FILTER_EXPR, ids, cond);

  return ret;
error:
  parser_get_until_tval (parser, tv_rbrace);
  free_tree (ret);
  free_tree (ids);
  free_tree (cond);
  return error_mark_node;
}

/*
   matrix:
   \begin { tmatrix }
    [ expr [ & expr ]* \lend ]+
   \end { tmatrix }
 */
static tree
handle_matrix (struct eq_parser * parser)
{
  tree list = error_mark_node;
  tree t = error_mark_node;
  struct eq_token *tok;
  struct eq_location loc;

  if (!(tok = parser_forward_tval (parser, tv_begin)))
    goto shift;
  else
    loc = eq_token_location (tok);

  if (!parser_forward_tval (parser, tv_lbrace))
    goto shift;
  if (!parser_forward_tval (parser, tv_tmatrix))
    goto shift;
  if (!parser_forward_tval (parser, tv_rbrace))
    goto shift;

  list = make_tree_list ();
  do
    {
      if (eq_token_is_keyword (parser_get_token (parser), tv_end))
	{
	  parser_unget (parser);
	  break;
	}
      else
	parser_unget (parser);

      t = handle_list (parser, handle_expr, tv_delimiter);
      tree_list_append (list, t);
    }
  while (eq_token_value (tok = parser_get_token (parser)) == tv_lend);

  if (!parser_forward_tval (parser, tv_end))
    goto error;

  if (!parser_forward_tval (parser, tv_lbrace))
    goto error;
  if (!parser_forward_tval (parser, tv_tmatrix))
    goto error;
  if (!parser_forward_tval (parser, tv_rbrace))
    goto error;

  return make_matrix (list, loc);

shift:
  while (true)
    {
      parser_get_until_tval (parser, tv_end);
      if (eq_token_value (parser_get_token (parser)) != tv_lbrace)
	continue;
      if (eq_token_value (parser_get_token (parser)) != tv_tmatrix)
	continue;
      if (eq_token_value (parser_get_token (parser)) != tv_rbrace)
	continue;
      break;
    }
error:
  free_tree (list);
  free_tree (t);
  return error_mark_node;
}

/*
   genarray:
   \genar \limits ^ { expr } ( expr )
 */
static tree
handle_genarray (struct eq_parser * parser)
{
  tree lim = error_mark_node;
  tree exp = error_mark_node;
  struct eq_token *tok;
  struct eq_location loc;

  if (!(tok = parser_forward_tval (parser, tv_genar)))
    goto shift;
  else
    loc = eq_token_location (tok);
  if (!parser_forward_tval (parser, tv_limits))
    goto shift;
  if (!parser_forward_tval (parser, tv_circumflex))
    goto shift;
  if (!parser_forward_tval (parser, tv_lbrace))
    goto shift;

  lim = handle_expr (parser);

  if (!parser_forward_tval (parser, tv_rbrace))
    goto shift;
  if (!parser_forward_tval (parser, tv_lparen))
    goto shift;

  exp = handle_expr (parser);

  if (!parser_forward_tval (parser, tv_rparen))
    goto error;

  return make_genar (lim, exp, loc);

shift:
  parser_get_until_tval (parser, tv_rparen);
error:
  free_tree (lim);
  free_tree (exp);
  return error_mark_node;
}

/*
   expr:
   ( sexpr | filter | genarray ) indexes
 */
static tree
handle_expr (struct eq_parser * parser)
{
  struct eq_token *tok;
  tree t;

  if (eq_token_is_keyword (tok = parser_get_token (parser), tv_filter))
    {
      parser_unget (parser);
      t = handle_filter (parser);
    }
  else if (eq_token_is_keyword (tok, tv_genar))
    {
      parser_unget (parser);
      t = handle_genarray (parser);
    }
  else
    {
      parser_unget (parser);
      t = handle_sexpr (parser);
    }

  return handle_indexes (parser, t);
}

/*
   return:
   \return { expr }
 */
static tree
handle_return (struct eq_parser * parser)
{
  tree t;
  tree ret;
  struct eq_token *tok;
  struct eq_location loc;

  if (!eq_token_is_keyword (tok = parser_get_token (parser), tv_return))
    goto shift;
  else
    loc = eq_token_location (tok);

  if (!parser_forward_tval (parser, tv_lbrace))
    goto shift;

  t = handle_list (parser, handle_expr, tv_comma);

  if (!parser_forward_tval (parser, tv_rbrace))
    goto error;

  ret = make_return (t, loc);
  return ret;
shift:
  parser_get_until_tval (parser, tv_rbrace);
error:
  return error_mark_node;
}

/*
   assign:
   idx [ , idx ]* \gets expr [ , expr ]*
 */
static tree
handle_assign (struct eq_parser * parser, tree prefix_id)
{
  tree id = NULL, expr = NULL;
  tree ret;
  struct eq_location loc;
  if (prefix_id == NULL)
    {
      id = handle_list (parser, handle_idx, tv_comma);
      loc = TREE_LOCATION (TREE_LIST (id)->entry);
    }
  else
    {
      id = prefix_id;
      loc = TREE_LOCATION (TREE_LIST (id)->entry);
    }

  if (!parser_forward_tval (parser, tv_gets))
    goto error;

  expr = handle_list (parser, handle_expr, tv_comma);

  ret = make_binary_op (ASSIGN_STMT, id, expr);
  TREE_LOCATION (ret) = loc;

  return ret;
error:
  free_tree (id);
  free_tree (expr);
  return error_mark_node;
}

/*
   declare:
   idx [ , idx ]* \in ext_type [, ext_type ]*
 */
static tree
handle_declare (struct eq_parser * parser, tree prefix_id)
{
  tree id = NULL, type = NULL;

  if (prefix_id == NULL)
    id = handle_list (parser, handle_id, tv_comma);
  else
    id = prefix_id;

  if (!parser_forward_tval (parser, tv_in))
    goto error;

  type = handle_list (parser, handle_stream_or_ext_type, tv_comma);

  return make_binary_op (DECLARE_STMT, id, type);

error:
  free_tree (id);
  free_tree (type);
  return error_mark_node;
}

/*
   instr:
   assign | declare | return | if_cond
 */
static tree
handle_instr (struct eq_parser * parser)
{
  struct eq_token *tok;
  tree idx;

  if (eq_token_is_keyword (tok = parser_get_token (parser), tv_return))
    {
      parser_unget (parser);
      return handle_return (parser);
    }
  else if (eq_token_is_keyword (tok, tv_match))
    {
      parser_unget (parser);
      if (!handle_match (parser))
	return error_mark_node;
      else
	return NULL;
    }
  else if (eq_token_is_keyword (tok, tv_comment))
    {
      parser_unget (parser);
      return handle_comment (parser);
    }
  else if (eq_token_is_keyword (tok, tv_print))
    {
      parser_unget (parser);
      return handle_print (parser);
    }
  else if (eq_token_is_keyword (tok, tv_qif))
    {
      tree ret;
      parser_unget (parser);
      ret = handle_if_cond (parser);
      return ret;
    }
  else if (eq_is_id (tok, false))
    {
      parser_unget (parser);
      idx = handle_list (parser, handle_idx, tv_comma);

      if (eq_token_is_operator (tok = parser_get_token (parser), tv_gets))
	{
	  parser_unget (parser);
	  return handle_assign (parser, idx);
	}
      else if (eq_token_is_operator (tok, tv_in))
	{
	  parser_unget (parser);
	  return handle_declare (parser, idx);
	}
      else if (eq_token_is_operator (tok, tv_vertical))
	{
	  parser_unget (parser);
	  return handle_parallel_loop (parser, idx);
	}

	free_tree (idx);
    }

  error_loc (eq_token_location (tok), "unexpected token `%s' ",
	     eq_token_as_string (tok));
  return error_mark_node;
}

/*
   parallel_loop:
   idx | generator \gets ( expr | parallel_loop_cases )
 */
static tree
handle_parallel_loop (struct eq_parser * parser, tree prefix_id)
{
  tree t = NULL, idx = NULL, cond = NULL, expr = NULL;

  if (prefix_id == NULL)
    {
      idx = handle_idx (parser);
      if (idx == NULL || idx == error_mark_node)
	goto error;
    }
  else
    {
      if (TREE_CODE (prefix_id) == LIST)
	{
	  if (TREE_LIST (prefix_id)->next != NULL)
	    {
	      error_loc (TREE_LOCATION (TREE_LIST (prefix_id)->next->entry),
		"only one identifier can be iterated inside one statement");
	    }
	  idx = eliminate_list (prefix_id);
	}
      else
	idx = prefix_id;
    }

  if (!parser_forward_tval (parser, tv_vertical))
    goto error;

  cond = handle_generator (parser);
  if (cond == NULL || cond == error_mark_node)
    goto error;

  if (!parser_forward_tval (parser, tv_gets))
    goto error;

  if (!eq_token_is_keyword (parser_get_token (parser), tv_begin))
    {
      tree tlist;
      parser_unget (parser);
      expr = handle_expr (parser);
      if (cond == NULL || cond == error_mark_node)
	goto error;

      tlist = make_tree_list ();
      tree_list_append (tlist,
			make_binary_op (CASE_EXPR, expr,
					make_tree (OTHERWISE_EXPR)));
      t = make_parallel_loop (idx, cond, tlist);
    }
  else
    {
      parser_unget (parser);
      expr = handle_parallel_loop_cases (parser);
      t = make_parallel_loop (idx, cond, expr);
    }
  return t;
error:
  free_tree (idx);
  free_tree (cond);
  free_tree (expr);
  free_tree (t);
  return error_mark_node;
}

/*
   parallel_loop_cases:
 */
static tree
handle_parallel_loop_cases (struct eq_parser * parser)
{
  tree list = NULL, expr = NULL, gen = NULL;

  if (!parser_forward_tval (parser, tv_begin))
    goto error;
  if (!parser_forward_tval (parser, tv_lbrace))
    goto error;
  if (!parser_forward_tval (parser, tv_cases))
    goto error;
  if (!parser_forward_tval (parser, tv_rbrace))
    goto error;

  list = make_tree_list ();
  while (true)
    {
      expr = handle_expr (parser);

      if (expr == NULL || expr == error_mark_node)
	goto error;

      if (!parser_forward_tval (parser, tv_delimiter))
	goto error;

      if (eq_token_is_keyword (parser_get_token (parser), tv_otherwise))
	{
	  gen = make_tree (OTHERWISE_EXPR);

	  if (!eq_token_is_keyword (parser_get_token (parser), tv_lend))
	    parser_unget (parser);

	  tree_list_append (list, make_binary_op (CASE_EXPR, expr, gen));
	  break;
	}
      else
	{
	  parser_unget (parser);
	  gen = handle_generator (parser);

	  if (!parser_forward_tval (parser, tv_lend))
	    goto error;
	  tree_list_append (list, make_binary_op (CASE_EXPR, expr, gen));
	}
    }

  if (is_end (parser, tv_cases))
    return list;

error:
  free_tree (list);
  free_tree (gen);
  free_tree (expr);
  return error_mark_node;
}

/*
   numx:
   num | divide
 */
static tree
handle_numx (struct eq_parser * parser)
{
  struct eq_token *tok;
  tree t;

  tok = parser_get_token (parser);
  if (eq_token_class (tok) == tok_intnum)
    t = make_integer_tok (tok);
  else if (eq_token_class (tok) == tok_realnum)
    t = make_real_tok (tok);
  else
    {
      parser_unget (parser);
      t = handle_divide (parser);
    }

  return handle_indexes (parser, t);
}

/*
   idx_numx:
   numx | idx
 */
static tree
handle_idx_numx (struct eq_parser * parser)
{
  tree t;
  struct eq_token *tok;
  tok = parser_get_token (parser);

  if (eq_is_id (tok, false))
    {
      parser_unget (parser);
      t = handle_idx (parser);
    }
  else
    {
      parser_unget (parser);
      t = handle_numx (parser);
    }
  return t;
}

/* Top level function to parse the file.  */
int
eq_parse (struct eq_parser *parser)
{
  struct eq_token *tok;
  eq_error_count = eq_warning_count = 0;
  while (eq_token_class (tok = parser_get_token (parser)) != tok_eof)
    {
      parser_unget (parser);

      if (eq_token_is_keyword (tok, tv_match))
	{
	  /* Enable lexer error handling inside match.  */
	  parser->lex->error_notifications = true;
	  handle_match (parser);
	  parser->lex->error_notifications = false;
	}
      else if (eq_token_is_keyword (tok, tv_proto))
	{
	  tree t = handle_proto (parser);
	  if (t != NULL && t != error_mark_node)
	    {
	      if (function_exists (
		TREE_STRING_CST (TREE_ID_NAME (TREE_FUNC_NAME (t)))))
		{
		  error_loc (TREE_LOCATION (t),
			"function `%s' is defined already",
			TREE_STRING_CST (TREE_ID_SOURCE_NAME
					 (TREE_FUNC_NAME (t))));
		  free_tree (t);
		}
	      else if (function_proto_exists (
		TREE_STRING_CST (TREE_ID_NAME (TREE_FUNC_NAME (t)))))
		{
		  error_loc (TREE_LOCATION (t),
			"prototype `%s' is defined already",
			TREE_STRING_CST (TREE_ID_SOURCE_NAME
					 (TREE_FUNC_NAME (t))));
		  free_tree (t);
		}
	      else
		tree_list_append (function_proto_list, t);
	    }
	}
      else
	{
	  /* Enable lexer error handling inside functions.  */
	  parser->lex->error_notifications = true;
	  tree t = handle_function (parser);
	  if (t != NULL && t != error_mark_node)
	    {
	      if (!function_exists (
		TREE_STRING_CST (TREE_ID_NAME (TREE_FUNC_NAME (t)))))
		tree_list_append (function_list, t);
	      else
		{
		  error_loc (TREE_LOCATION (t),
			"function `%s' is defined already",
			TREE_STRING_CST (TREE_ID_SOURCE_NAME
					  (TREE_FUNC_NAME (t))));
		}
	    }
	  parser->lex->error_notifications = false;
	}
    }

  printf ("note: finished parsing.\n");
  if (eq_error_count != 0)
    {
      printf ("note: %i errors found.\n", eq_error_count);
      return -3;
    }

  return 0;
}

/* I know this is ugly, but life is tough!  */
#include "matcher.c"
