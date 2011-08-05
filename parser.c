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
#include "tree.h"
#include "global.h"
#include "print.h"


struct parser 
{
  struct lexer *lex;
  
  /* Buffer and lengths associated with buffer. 
     Buffer holds up-to BUF_SIZE tokens, which means
     that it is possible to look BUF_SIZE tokens 
     forward.  */
  struct token **token_buffer;
  size_t buf_size ;
  size_t buf_start, buf_end, unget_idx;
  bool buf_empty;
  
  /* Count of opened parens, square brackets and 
     figure brackets. Used when we skip the tokens
     skip is finished when all the three counters
     are zeroes.  */
  int paren_count;
  int square_count;
  int brace_count;
};

/* Check if parser is not in any parenthesis/bracket expression.  */
static inline bool
parser_parens_zero (struct parser *parser)
{
  return parser->paren_count == 0 
         && parser->square_count == 0
         && parser->brace_count == 0;
}


struct token *  parser_get_token (struct parser *);
void parser_unget (struct parser *);
struct token *  parser_get_until_tval (struct parser *, enum token_kind);
struct token *  parser_get_until_tclass (struct parser *, enum token_class);
struct token * parser_forward_tval (struct parser *, enum token_kind);
struct token * parser_forward_tclass (struct parser *, enum token_class);
bool parser_expect_tval (struct parser *, enum token_kind);
bool parser_expect_tclass (struct parser *, enum token_class);
bool parser_init (struct parser *, struct lexer *);
bool parser_finalize (struct parser *);

/* Get one token from the lexer or from the token buffer.
   Token is taken from the buffer if parser_unget was 
   called earlier. */
struct token *
parser_get_token (struct parser *  parser)
{
  struct token *  tok;
  
  if (parser->unget_idx == 0)
    {
      /* Skip comments for the time being. We do not skip
         the comments at the level of lexer, because we 
         can put them in the output program.  */
      while (true)
        {
          tok = lexer_get_token (parser->lex);
          if (token_class (tok) != tok_comments)
            break;
          else
            token_free (tok);
        }
            
      /* Keep track of brackets.  */
      if (token_class (tok) == tok_operator)
        switch (token_value (tok))
          {
          case tv_lparen:
            parser->paren_count ++; break;
          case tv_rparen:
            parser->paren_count --; break;
          case tv_lsquare:
            parser->square_count ++; break;
          case tv_rsquare:
            parser->square_count --; break;
          case tv_lbrace:
            parser->brace_count ++; break;
          case tv_rbrace:
            parser->brace_count --; break;
          default:
            ;
          }
      
      /* If TOKEN_BUFFER is full, we free the token pointed by BUF_START
         and put the new token on its place, changing BUF_START and
         BUF_END accordingly.  */
      if ((parser->buf_end + 1) % parser->buf_size == parser->buf_start)
        {
          token_free (parser->token_buffer[parser->buf_start]);
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
              "parser buffer holds only up to %i values.",
              parser->buf_size);
      
      s = parser->buf_end - parser->unget_idx;
      s = s < 0 ? parser->buf_size + s : s;
      parser->unget_idx --;
      
      tok = parser->token_buffer[s];
    }

  return tok;
}


/* Move the parser one token back. It means that the consequent
   call of parser_get_token would return the token from buffer,
   not from lexer.  */
void
parser_unget (struct parser *  parser)
{
  parser->unget_idx++;
  assert (parser->unget_idx < parser->buf_size, 
          "parser buffer holds only up to %i values.",
          parser->buf_size);
}


/* Skip tokens until token with value TKIND would be found.  */
struct token *
parser_get_until_tval (struct parser * parser, enum token_kind tkind)
{
  struct token * tok;
  
  do 
    {
      tok = parser_get_token (parser);
      if (!token_uses_buf (token_class (tok))
          /* FIXME the following condition makes it impossible
             to skip until some symbol if you are inside the 
             block or brackets. */
          /* && parser_parens_zero (parser) */
          && token_value (tok) == tkind)
        {
          return tok;
        }
    }
  while (token_class (tok) != tok_eof);

  return tok;
}

/* Skip tokens until token of class TCLASS would be found.  */
struct token *
parser_get_until_tclass (struct parser * parser, enum token_class tclass)
{
  struct token * tok;
  
  do 
    {
      tok = parser_get_token (parser);
      /* FIXME the following condition makes it impossible
         to skip until some symbol if you are inside the 
         block or brackets. */
      if (/* parser_parens_zero (parser) && */ token_class (tok) == tclass)
        {
          return tok;
        }
    }
  while (token_class (tok) != tok_eof);

  return tok;
}


/* Get the next token and check if it's value  is what expected.
   Function doesn't unget the token in case the success.
   In case unexpected value print error message */
struct token *
parser_forward_tval (struct parser *parser, enum token_kind tkind)
{
  struct token * tok = parser_get_token (parser);

  if (token_value(tok) != tkind)
    {
      error_loc (token_location (tok), "unexpected token `%s` ", token_as_string (tok));
      return NULL;
    }
  return tok;
}

/* Get the next token and check if it's class is what expected.
   Function doesn't unget the token in case the success.
   In case unexpected class print error message */
struct token *
parser_forward_tclass (struct parser *parser, enum token_class tclass)
{
  struct token * tok = parser_get_token (parser);

  if (token_class(tok) != tclass)
    {
      error_loc (token_location (tok), "unexpected token `%s` ", token_as_string (tok));
      return NULL;
    }
  return tok;
}

/* Get the next token from two alternative class options.
   If the token is different, return NULL
 */
struct token *
parser_token_alternative_tclass (struct parser *parser, enum token_class first, enum token_class second)
{
  struct token* tok = parser_get_token (parser);
  
  if ((token_class(tok) == first) || (token_class(tok) == second))
    return tok;
  
  parser_unget (parser);
  return NULL;
}

/* Get the next token from two alternative valueoptions.
   If the token is different, return NULL
 */
struct token *
parser_token_alternative_tval (struct parser *parser, enum token_kind first, enum token_kind second)
{
  struct token* tok = parser_get_token (parser);
  
  if ((token_value(tok) == first) || (token_value(tok) == second))
    return tok;
  
  parser_unget (parser);
  return NULL;
}



/* Check if the next token returned by parser_get_token would be
   token with the value TKIND, in case the value is different,
   the error_loc would be called.
   NOTE: function ungets the token after checking it.  */
bool
parser_expect_tval (struct parser *parser, enum token_kind tkind)
{
  struct token *  tok = parser_get_token (parser);
  if (!token_uses_buf (token_class (tok)) && token_value (tok) == tkind)
    {
      parser_unget (parser);
      return true;
    }
  else
    {
      error_loc (token_location (tok), 
                 "token `%s' expected, `%s' token found",
                 token_kind_as_string (tkind), token_as_string (tok));
      parser_unget (parser);
      return false;
    }
}

/* Check if the next token returned by parser_get_token would be
   token of class TCLASS, in case the class is different,
   the error_loc would be called.
   NOTE: function ungets the token after checking it.  */
bool
parser_expect_tclass (struct parser *parser, enum token_class tclass)
{
  struct token *  tok = parser_get_token (parser);
  if (token_class (tok) == tclass)
    {
      parser_unget (parser);
      return true;
    }
  else
    {
      error_loc (token_location (tok), 
                 "token of class `%s' expected, `%s' token found",
                 token_class_as_string (tclass), token_as_string (tok));
      parser_unget (parser);
      return false;
    }
}


/* Initialize the parser, allocate memory for token_buffer.  */
bool
parser_init (struct parser * parser, struct lexer *lex)
{
  parser->lex = lex;
  parser->buf_size = 16;
  parser->buf_start = 0;
  parser->buf_end = 0;
  parser->buf_empty = true;
  parser->token_buffer 
    = (struct token **) malloc (parser->buf_size * sizeof (struct token *));
  parser->unget_idx = 0;
  return true;
}


/* Clear the memory allocated for internal structure.
   NOTE: PARSER is not freed.  */
bool
parser_finalize (struct parser *parser)
{
  assert (parser, "attempt to free empty parser");

  while (parser->buf_start % parser->buf_size 
         != parser->buf_end % parser->buf_size)

    {
      token_free (parser->token_buffer[parser->buf_start]);
      parser->buf_start = (parser->buf_start + 1) % parser->buf_size;
    }

  if (parser->token_buffer) 
    free (parser->token_buffer);
  
  lexer_finalize (parser->lex);
  return true;
}

/* documentclass:
   \documentclass { <num> <id>, <id> } { <id> }
   FIXME: In LaTeX the first argument should be <num><id>, i.e. 12pt,
   where spaces have to be missing. However, in this grammar spaces 
   are acceptable.
 */
tree
handle_documentclass (struct parser *parser)
{
  tree t;
  struct token * tok;

  if (!(tok = parser_forward_tval(parser, tv_documentclass)))
    return error_mark_node;

  if (!(tok = parser_forward_tval(parser, tv_lbrace)))
    return error_mark_node;
  
  t = make_tree (DOCUMENTCLASS);

  if (!(tok = parser_forward_tclass(parser, tok_number)))
    goto error;
  else
    TREE_OPERAND_SET(t, 0, make_integer_tok(tok));
  
  if (!(tok = parser_forward_tclass(parser, tok_id)))
    goto error;
  else
    TREE_OPERAND_SET(t, 1, make_string_cst_tok (tok));

  if (!(tok = parser_forward_tval(parser, tv_comma)))
    goto error;

  if (!(tok = parser_forward_tclass(parser, tok_id)))
    goto error;
  else
    TREE_OPERAND_SET(t, 2, make_string_cst_tok (tok));

  if (!(tok = parser_forward_tval(parser, tv_rbrace)))
    goto error;
  
  if (!(tok = parser_forward_tval(parser, tv_lbrace)))
    goto error;

  if (!(tok = parser_forward_tclass(parser, tok_id)))
    goto error;
  else
    TREE_OPERAND_SET(t, 3, make_string_cst_tok (tok));

  if (!(tok = parser_forward_tval(parser, tv_rbrace)))
    goto error;
 
  return t;

error:
    free_tree(t);
    return error_mark_node;
}

/* usepackage:
   \usepackage { <id> }
 */
tree
handle_usepackage (struct parser *parser)
{
  tree t;
  struct token * tok;

  if (!(tok = parser_forward_tval(parser, tv_usepackage)))
    return error_mark_node; 
  
  if (!(tok = parser_forward_tval(parser, tv_lbrace)))
    return error_mark_node;
  
  t = make_tree (USEPACKAGE);

  if (!(tok = parser_forward_tval(parser, tv_eqcode)))
    goto error;
  else
    TREE_OPERAND_SET(t, 0, make_string_cst_tok (tok));

  if (!(tok = parser_forward_tval(parser, tv_rbrace)))
    goto error;
  
  return t;

error:
    free_tree(t);
    return error_mark_node;
}

/* begindocument:
 * \begin{ document }
 */
tree
handle_begindocument (struct parser *parser)
{
  tree t;
  struct token * tok;

  if (!(tok = parser_forward_tval(parser, tv_begin)))
    return error_mark_node; 
  
  if (!(tok = parser_forward_tval(parser, tv_lbrace)))
    return error_mark_node;
  
  t = make_tree (BEGIN);

  if (!(tok = parser_forward_tval(parser, tv_document)))
    goto error;
  else
    TREE_OPERAND_SET(t, 0, make_string_cst_tok (tok));

  if (!(tok = parser_forward_tval(parser, tv_rbrace)))
    goto error;
  
  return t;

error:
    free_tree(t);
    return error_mark_node;
}

/* footer:
 * \end { document }
 */
tree
handle_footer (struct parser *parser)
{
  tree t;
  struct token * tok;

  if (!(tok = parser_forward_tval(parser, tv_begin)))
    return error_mark_node; 
  
  if (!(tok = parser_forward_tval(parser, tv_lbrace)))
    return error_mark_node;
  
  t = make_tree (END);

  if (!(tok = parser_forward_tval(parser, tv_document)))
    goto error;
  else
    TREE_OPERAND_SET(t, 0, make_string_cst_tok (tok));

  if (!(tok = parser_forward_tval(parser, tv_rbrace)))
    goto error;
  
  return t;

error:
    free_tree(t);
    return error_mark_node;
}


/* header:
 * documentclass usepackage begindocument
 */
static inline void handle_header (struct parser* parser)
{
  handle_documentclass (parser);
  handle_usepackage (parser);
  handle_begindocument (parser);
}

/* linear:
 * <id> [ ( + | - ) <num> ]
 */
tree
handle_linear (struct parser* parser)
{
  tree id;
  struct token* tok;

  if (!(tok = parser_forward_tclass(parser, tok_id)))
    return error_mark_node;
  else
    id = make_identifier_tok (tok);

  if (!(tok = parser_token_alternative_tval(parser, tv_plus, tv_minus)))
  {
    enum token_kind op = token_value(tok);
    tok = parser_get_token(parser);
    if(token_class(tok) != tok_number)
    {
      parser_unget(parser);
      parser_unget(parser);
      return id;
    }
    else
      return make_binary_op (op, id, make_integer_tok(tok));
  }
  return id; 
}

/* Top level function to parse the file.  */
int
parse (struct parser *parser)
{
  struct token *  tok;
  
  error_count = warning_count = 0;
  while (token_class (tok = parser_get_token (parser)) != tok_eof)
    {
      switch (token_class (tok))
        {
        case tok_keyword:
          switch (token_value (tok))
            {
            case tv_documentclass:
              parser_unget (parser);
              handle_header (parser);
              break;
            default:
              error_loc( token_location (tok), "keyword `%s` is not expected here",
                  token_as_string(tok));
            }
          break;
        case tok_unknown:
          error_loc( token_location (tok), "unknown token found `%s`", 
              token_as_string(tok));
          break;
        default:
          error_loc (token_location (tok), "token `%s` is not expected here",
            token_as_string(tok));
        }
    }

            
  printf ("note: finished parsing.\n");
  if (error_count != 0)
    {
      printf ("note: %i errors found.\n", error_count);
      return -3;
    }

  return 0;
}


int 
main (int argc, char *argv[])
{
  int ret = 0;
  struct lexer *  lex = (struct lexer *) malloc (sizeof (struct lexer));
  struct parser *  parser = (struct parser *) malloc (sizeof (struct parser));

  init_global ();
  init_global_tree ();
  /* init_function_protos (); */ 
  if (argc <= 1)
    {
      fprintf (stderr, "filename argument required\n");
      ret = -1;
      goto cleanup;
    }

  if (!lexer_init (lex, argv[1]))
    {
      fprintf (stderr, "cannot create a lexer for file %s\n", argv[1]);
      ret = -2;
      goto cleanup;
    }

  parser_init (parser, lex);
  parse (parser);

  /*if (error_count == 0)
    print_all (stdout);*/
  
  print_all (stdout);

cleanup:
  parser_finalize (parser);
  finalize_global_tree ();
  finalize_global ();
  
  /* That should be called at the very end.  */
  free_atomic_trees ();

  if (parser)
    free (parser);
  if (lex)
    free (lex);
  
  return ret == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}