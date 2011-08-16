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

struct token * parser_get_until_tval (struct parser *, enum token_kind);
struct token * parser_get_until_tclass (struct parser *, enum token_class);
struct token * parser_forward_tval (struct parser *, enum token_kind);
struct token * parser_forward_tclass (struct parser *, enum token_class);
struct token * parser_toke_alternative_tval(struct parser *, enum token_kind, enum token_kind);
struct token * parser_toke_alternative_tclass(struct parser *, enum token_class, enum token_class);
bool parser_expect_tval (struct parser *, enum token_kind);
bool parser_expect_tclass (struct parser *, enum token_class);
bool parser_init (struct parser *, struct lexer *);
bool parser_finalize (struct parser *);
bool is_id (struct token *, bool);
tree handle_type (struct parser *);
tree handle_ext_type (struct parser *);
tree handle_list (struct parser *, tree (*)(struct parser*), enum token_kind);
tree handle_ext_type_or_ext_type_list (struct parser*);
tree handle_sexpr_or_sexpr_list (struct parser*);
tree handle_id (struct parser *);
tree handle_idx (struct parser *);
tree handle_idx_or_idx_list (struct parser *);
tree handle_lower (struct parser *);
tree handle_upper (struct parser *);
tree handle_function (struct parser *);
tree handle_function_call (struct parser *);
tree handle_linear (struct parser *);
tree handle_divide (struct parser *);
tree handle_sexpr (struct parser *);
tree handle_sexpr_op (struct parser *);
tree handle_condition (struct parser *);
tree handle_filter (struct parser *);
tree handle_matrix (struct parser *);
tree handle_vector (struct parser *);
tree handle_genarray (struct parser *);
tree handle_expr (struct parser *);
tree handle_return (struct parser *);
tree handle_assign (struct parser *);
tree handle_declare (struct parser *);
tree handle_instr (struct parser *);
tree handle_with_loop (struct parser *);
tree handle_with_loop_cases (struct parser *);

int parse(struct parser *);

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
  struct token* tok = parser_get_token(parser);
 
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
  {
    return tok;
  }

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

/* Check either token is a valid id */
bool
is_id (struct token * tok, bool error)
{
  int ret = false;
  if (token_class (tok) == tok_id)
    return true;
  if (token_class (tok) == tok_keyword)
  {
    ret =  is_token_id [ (int) (tok->value.tval - tv_boolean)];
  }
  if (error && !ret)
    error_loc (token_location (tok), "unexpected token `%s` ", token_as_string (tok));
  return ret;
}

/*
  type:
  \type { (Z | R | N | B) }
 */
tree
handle_type (struct parser* parser)
{
  tree t;
  struct token * tok;
 
  if(!(tok = parser_forward_tval (parser, tv_type)))
    goto error;

  if(!(tok = parser_forward_tval (parser, tv_lbrace)))
    goto error;
  
  tok = parser_get_token (parser);
  if(token_value(tok) == tv_boolean)
    t = make_type (B_TYPE);
  else if (token_value (tok) == tv_natural)
    t = make_type (N_TYPE);
  else if (token_value (tok) == tv_integer)
    t = make_type (Z_TYPE);
  else if (token_value (tok) == tv_real)
    t = make_type (R_TYPE);
  else
    goto error;

  TREE_TYPE_DIM(t) = NULL;
  TREE_TYPE_SHAPE(t) = NULL;
  TREE_LOCATION(t) = token_location(tok);

  if(!(tok = parser_forward_tval (parser, tv_rbrace)))
    return error_mark_node;

  return t;
error:
  parser_get_until_tval(parser, tv_rbrace);
  return error_mark_node;
}

/*
  ext_type:
  type [ ^ { sexpr } [ _ { sexpr [ , sexpr ]* } ] ]
 */
tree
handle_ext_type (struct parser * parser)
{
  struct token * tok;
    
  tree t = handle_type (parser);
  tree dim, shape;
  
  tok = parser_get_token(parser);
  if (token_value(tok) != tv_circumflex)
  {
    parser_unget (parser);
    return t;
  }

  if (!(tok = parser_forward_tval (parser, tv_lbrace)))
    goto error_shift;

  dim = handle_sexpr(parser);

  if (dim == NULL || dim == error_mark_node)
    goto error_shift;
  else
    TREE_TYPE_DIM(t) = dim;
 
  if(!(tok = parser_forward_tval(parser, tv_rbrace)))
    goto error_shift;

  tok = parser_get_token (parser);

  if (token_value(tok) != tv_lower_index)
  {
    parser_unget(parser);
    return t;
  }

  if (!(tok = parser_forward_tval(parser, tv_lbrace)))
    goto error_shift;

  shape = handle_sexpr_or_sexpr_list (parser);

  if (shape == NULL || shape == error_mark_node)
    goto error;
  else
    TREE_TYPE_SHAPE(t) = shape;

  if(!(tok = parser_forward_tval(parser, tv_rbrace)))
  {
    goto error;
  }
  
  return t;
error_shift:
  parser_get_until_tval(parser, tv_rbrace);
error:
  free_tree(t);
  free_tree(dim);
  free_tree(shape);
  return error_mark_node;
}

tree
handle_id (struct parser * parser)
{
  struct token * tok;
  tree t;

  if (!is_id (tok = parser_get_token (parser), true))
    return error_mark_node;
  else
    t = make_identifier_tok ( tok);
  return t;
}

/*
 * idx:
 * ( divide | <num> | <id> [ upper ] [ lower ] )
 * TODO: lower is not being parsed at the moment
 */
tree
handle_idx (struct parser* parser)
{
  tree idx, t;
  struct token* tok;
    
  tok = parser_get_token (parser);
  
  if (is_id (tok, false)) 
    t = make_identifier_tok (tok);
  else if (token_class(tok) == tok_number)
    t = make_integer_tok (tok);
  else if (   token_value(tok) == tv_frac 
           || token_value(tok) == tv_dfrac)
  {
    parser_unget(parser);
    t = handle_divide (parser);
  }
  else 
    return error_mark_node;

  tok = parser_get_token(parser);

  if (token_value(tok) == tv_circumflex)
  {
    parser_unget(parser);
    idx = handle_upper(parser);
    if (idx != NULL && idx != error_mark_node)
      TREE_OPERAND_SET(idx, 0, t);
    else
      return error_mark_node;
  }
  else
  {
    parser_unget(parser);
    idx = t;
  }


  tok = parser_get_token(parser); 
  if (token_value(tok) == tv_lower_index)
  { 
    parser_unget(parser);
    t = handle_lower(parser);
    
    if (t != NULL && t != error_mark_node)
    {
      TREE_OPERAND_SET(t, 0, idx);
    
      return t;
    }
    else
      return error_mark_node;

  }
  else
    parser_unget(parser);


  return idx;
}

tree 
handle_list (struct parser * parser, tree (*handler)(struct parser*), enum token_kind delim)
{
  tree ret, t;
  struct token * tok;
  t = (*handler) (parser);

  if(token_value(tok = parser_get_token(parser)) != delim)
  {

    ret = t;
  }
  else
  {
    tree list = make_tree_list();
    tree_list_append (list, t);
    while (true)
    {
      t = (*handler)(parser);

      if (t != NULL && t != error_mark_node)
      {
        tree_list_append(list, t);
      }
      tok = parser_get_token(parser);
      if (token_value(tok) != delim)
        break;
    }
    ret = list;
  }
  parser_unget(parser);
  
  return ret;
}

/*
 * Read ext_type or ext_type
 * ext_type [ , ext_type ]*
 */
tree 
handle_ext_type_or_ext_type_list (struct parser * parser)
{
  return handle_list (parser, handle_ext_type, tv_comma);
}

/*
 * Read sexp or sexpr list
 * sexpr [ , sexpr ]*
 */
tree 
handle_sexpr_or_sexpr_list (struct parser * parser)
{
  return handle_list (parser, handle_sexpr, tv_comma);
}

/*
 * Read idx or idx list
 * idx  [ , idx ]*
 */
tree 
handle_idx_or_idx_list (struct parser * parser)
{
  return handle_list(parser, handle_idx, tv_comma);
}

/*
 *  lower
 *  _ { sexpr [ , sexpr ]* }
 */
tree 
handle_lower (struct parser* parser)
{
  tree lower, t;
  struct token * tok;

  if (!(tok = parser_forward_tval(parser, tv_lower_index)))
  {
    parser_get_until_tval(parser, tv_rbrace);
    return error_mark_node;
  }

  if (!(tok = parser_forward_tval(parser, tv_lbrace)))
  {
    parser_get_until_tval(parser, tv_rbrace);
    return error_mark_node;
  }

  t = handle_sexpr_or_sexpr_list (parser);
  
  if (t == NULL || t == error_mark_node)
    goto error_shift;
  else
    lower = make_binary_op(LOWER, NULL, t);
  
  if (!(tok = parser_forward_tval(parser, tv_rbrace)))
  {
    parser_unget(parser);
    goto error;
  }

  return lower;

error_shift:
  parser_get_until_tval(parser, tv_rbrace);

error:
  free_tree(lower);
  return error_mark_node; 

}

bool
is_end (struct parser * parser, enum token_kind tok)
{
  if (token_value(parser_get_token(parser)) == tv_end)
  {
    if(token_value(parser_get_token(parser)) == tv_lbrace)
    {
      if(token_value(parser_get_token(parser)) == tok)
      {
        if(token_value(parser_get_token(parser)) == tv_rbrace)
        {
          return true;
        }
        parser_unget(parser);
      }
    }
    parser_unget(parser);
  }
  parser_unget(parser);
  return false;
}
/*
 * function:
 * \begin { eqcode } { id }
 * { [ idx [ , idx ]* ] }
 * { [ ext_type [ , ext_type ]* ] }
 * { ext_type }
 * instr_list
 * \end { eqcode}
 */
tree 
handle_function ( struct parser * parser )
{
  tree name = NULL, args = NULL, arg_types = NULL, ret = NULL, instrs = NULL, t = NULL;
  struct token* tok;
  if (token_value(parser_get_token(parser)) != tv_begin)
    return NULL;
  if (token_value(parser_get_token(parser)) != tv_lbrace)
    return NULL;
  if (token_value(parser_get_token(parser)) != tv_eqcode)
    return NULL;
  if (token_value(parser_get_token(parser)) != tv_rbrace)
    return NULL;

  if (!(tok = parser_forward_tval(parser, tv_lbrace)))
    goto error;

  if (!is_id (tok = parser_get_token (parser), true))
    goto error;
  else
  {
    name = make_identifier_tok (tok);
  }

  if (!(tok = parser_forward_tval(parser, tv_rbrace)))
    goto error;

  if (!(tok = parser_forward_tval(parser, tv_lbrace)))
    goto error;

  if(token_value(parser_get_token(parser)) != tv_rbrace)
  {
    parser_unget(parser);
    args = handle_idx_or_idx_list (parser);
    if (args == error_mark_node)
      goto error;
  }
  else
  {
    parser_unget(parser);
    args = NULL;
  }

  if (!(tok = parser_forward_tval(parser, tv_rbrace)))
    goto error;

  if (!(tok = parser_forward_tval(parser, tv_lbrace)))
    goto error;

  if(token_value(parser_get_token(parser)) != tv_rbrace)
  {
    parser_unget(parser);
    arg_types = handle_ext_type_or_ext_type_list (parser);
    if (arg_types == error_mark_node)
      goto error;
  }
  else
  {
    parser_unget(parser);
    arg_types = NULL;
  }

  if (!(tok = parser_forward_tval(parser, tv_rbrace)))
    goto error;

  if (!(tok = parser_forward_tval(parser, tv_lbrace)))
    goto error;

  ret = handle_ext_type(parser);

  if(ret == NULL || ret == error_mark_node)
    goto error;

  if (!(tok = parser_forward_tval(parser, tv_rbrace)))
    goto error;

  instrs = make_tree_list (); 
  
  while(true)
  {
    tree t;

    /* end of file check */
    if (token_class(parser_get_token(parser)) == tok_eof)
      break;
    else
      parser_unget(parser);
    
    /* \end{eqcode} check */
    if (is_end(parser, tv_eqcode))
      break;

    t = handle_instr (parser);

    if (t == error_mark_node)
    {
      parser_get_until_tval (parser, tv_lend);
      continue;      
    }

    tree_list_append(instrs, t);

    if(parser_expect_tval(parser, tv_lend))
      parser_get_token(parser);

  }
  return make_function(name, args, arg_types, ret, instrs);

error:
  while(true)
  {
    parser_get_until_tval(parser, tv_end);
    if(token_value(parser_get_token(parser)) != tv_lbrace)
      continue;
    if(token_value(parser_get_token(parser)) != tv_eqcode)
      continue;
    if(token_value(parser_get_token(parser)) != tv_rbrace)
      continue;
    break;
  }
  free_tree(name);
  free_tree(args);
  free_tree(arg_types);
  free_tree(ret);
  free_tree(instrs);
  free_tree(t);
  return error_mark_node;
}

/*
 * function_call:
 * \call { <id> } { [ idx [, idx ]* ] }
 */
tree 
handle_function_call (struct parser* parser)
{
  tree t = NULL, args = NULL;
  struct token * tok;
  
  if (!(tok = parser_forward_tval(parser, tv_call)))
    goto error;

  if (!(tok = parser_forward_tval(parser, tv_lbrace)))
    goto error;

  t = make_tree (FUNCTION_CALL);

  if (!is_id (tok = parser_get_token (parser), true))
    goto error;
  else
    TREE_OPERAND_SET(t, 0, make_identifier_tok (tok));
  
  if(!(tok = parser_forward_tval(parser, tv_rbrace)))
    goto error;
  if(!(tok = parser_forward_tval(parser, tv_lbrace)))
    goto error;
 
  TREE_OPERAND_SET(t, 1, NULL);
  tok = parser_get_token(parser);
  if (is_id (tok, false) || token_class(tok) == tok_number)
  {
    parser_unget (parser);
    args = handle_idx_or_idx_list (parser);
    if (args != NULL && args != error_mark_node)
      TREE_OPERAND_SET(t, 1, args);     
    else
      goto error;
  }
  else if (token_value(tok) != tv_rbrace)
  {
    free_tree(t);
    return error_mark_node;
  }
  else
    return t;
 
  if(!(tok = parser_forward_tval(parser, tv_rbrace)))
  {
    free_tree(t);
    return error_mark_node;
  }

  return t;

error:
  parser_get_until_tval(parser, tv_rbrace);
  free_tree(t);
  return error_mark_node;

}

/*
 * upper:
 * ^ { [ ( [linear] | linear ) ] }
 */
tree
handle_upper (struct parser* parser)
{
  tree circumflex, t;
  struct token* tok;
   
  if (!(tok = parser_forward_tval(parser, tv_circumflex)))
  {
    parser_get_until_tval(parser, tv_rbrace);
    return error_mark_node;
  }

  if (!(tok = parser_forward_tval(parser, tv_lbrace)))
  {
    parser_get_until_tval(parser, tv_rbrace);
    return error_mark_node;
  }
  circumflex = make_tree(CIRCUMFLEX);
  TREE_OPERAND_SET(circumflex, 0, NULL);

  if (!(token_value(tok = parser_get_token(parser)) == tv_lsquare))
  {
    parser_unget (parser);
    circumflex->circumflex_node.is_index = false;
  }
  else
    circumflex->circumflex_node.is_index = true;

  t = handle_linear (parser);
  if (t == NULL || t == error_mark_node)
    goto error_shift;
  else
    TREE_OPERAND_SET(circumflex, 1, t);
  
  if (circumflex->circumflex_node.is_index)
  {
    if(!(tok = parser_forward_tval(parser, tv_rsquare)))
    {
      goto error_shift;
    }
  }

  if (!(tok = parser_forward_tval(parser, tv_rbrace)))
  {
    parser_unget(parser);
    goto error;
  }

  return circumflex;

error_shift:
  parser_get_until_tval(parser, tv_rbrace);

error:
  free_tree(circumflex);
  return error_mark_node; 
}

/* linear:
 * (<num> | <id> [ ( + | - ) <num> ] )
 */
tree
handle_linear (struct parser* parser)
{
  tree id;
  struct token* tok;

  if (!is_id (tok = parser_get_token (parser), false))
  {
    if(token_class(tok) != tok_number)
      return error_mark_node;
    else
    {
      return make_integer_tok(tok);
    }
  }
  else
    id = make_identifier_tok (tok);
  
  if ((tok = parser_token_alternative_tval(parser, tv_plus, tv_minus)) != NULL)
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
    {
      if (op == tv_plus) return make_binary_op (PLUS_EXPR, id, make_integer_tok(tok));
      else return make_binary_op (MINUS_EXPR, id, make_integer_tok(tok));
    }
  }

  return id; 
}

/*
 * ( \frac | \dfrac) { sexpr } { sexpr }
 */
tree 
handle_divide (struct parser * parser)
{
  tree t = NULL, l = NULL, r = NULL;

  struct token * tok = parser_token_alternative_tval(parser, tv_frac, tv_dfrac);
  
  if(tok == NULL)
  {
    error_loc (token_location (tok), "unexpected token `%s` ", token_as_string(tok));
    goto error_shift_one;
  }
  
  if(!(tok = parser_forward_tval(parser, tv_lbrace)))
    goto error_shift_one;

  l = handle_sexpr(parser);
  if (l == NULL || l == error_mark_node)
    goto error_shift_one;
  
  if(!(tok = parser_forward_tval(parser, tv_rbrace)))
    goto error_shift_two;

  if(!(tok = parser_forward_tval(parser, tv_lbrace)))
    goto error_shift_two;

  r = handle_sexpr(parser);
  if (r == NULL || r == error_mark_node)
    goto error_shift_two;

  t = make_binary_op(DIV_EXPR, l, r);

  if(!(tok = parser_forward_tval(parser, tv_rbrace)))
    goto error;

  return t;
error_shift_one:
  parser_get_until_tval(parser, tv_rbrace);
error_shift_two:
  parser_get_until_tval(parser, tv_rbrace);
error:
  free_tree(l);
  free_tree(r);
  return error_mark_node;
}

/* relations:
 * sexpr rel sexpr [ rel sexpr ]
 */
tree 
handle_relations (struct parser * parser)
{
  tree t1;
  tree t2, t3;
  tree rel1, rel2;
  struct token * tok;

  t1 = handle_sexpr (parser);
  if (t1 == NULL || t1 == error_mark_node)
    goto error;

  tok = parser_get_token(parser);
  
  if (token_class(tok) == tok_operator)
  {
    switch (token_value(tok))
    {
      case tv_lt:
        rel1 = make_binary_op(LT_EXPR, t1, NULL);
        break;
      case tv_gt:
        rel1 = make_binary_op(GT_EXPR, t1, NULL);
        break;
      case tv_geq:
        rel1 = make_binary_op(GE_EXPR, t1, NULL);
        break;
      case tv_leq:
        rel1 = make_binary_op(LE_EXPR, t1, NULL);
        break;
      case tv_not:
        if (!parser_forward_tval(parser, tv_eq))
          goto error;
        rel1 = make_binary_op(NE_EXPR, t1, NULL);
        break;
      case tv_eq:
        rel1 = make_binary_op(EQ_EXPR, t1, NULL);
        break;
      default:
        goto error;
    }
  }
  else
    goto error;

  t2 = handle_sexpr (parser);
  if (t2 == NULL || t2 == error_mark_node)
    goto error;
  
  TREE_OPERAND_SET(rel1, 1, t2);

  tok = parser_get_token(parser);

  if (token_class(tok) == tok_operator)
  {
    switch (token_value(tok))
    {
      case tv_lt:
        rel2 = make_binary_op(LT_EXPR, t2, NULL);
        break;
      case tv_gt:
        rel2 = make_binary_op(GT_EXPR, t2, NULL);
        break;
      case tv_geq:
        rel2 = make_binary_op(GE_EXPR, t2, NULL);
        break;
      case tv_leq:
        rel2 = make_binary_op(LE_EXPR, t2, NULL);
        break;
      case tv_not:
        if (!parser_forward_tval(parser, tv_eq))
          goto error;
        rel2 = make_binary_op(NE_EXPR, t2, NULL);
        break;
      case tv_eq:
        rel2 = make_binary_op(EQ_EXPR, t2, NULL);
        break;
      default:
        parser_unget(parser);
        return rel1;
    }
  }
  else
  {
    parser_unget (parser);
    return rel1;
  }

  t3 = handle_sexpr (parser);

  if (t3 == NULL || t3 == error_mark_node)
    goto error;

  TREE_OPERAND_SET(rel2, 1, t3);
  return make_binary_op(AND_EXPR, rel1, rel2);

error:
  free_tree(t1);
  free_tree(t2);
  free_tree(t3);
  free_tree(rel1);
  free_tree(rel2);
  return error_mark_node;
}

tree 
handle_cond_block (struct parser * parser)
{
  tree t;
  struct token * tok;
  enum prec {
    prec_none,
    prec_logor,
    prec_logand,
    num_precs
  };

  struct {
    tree expr;
    enum prec prec;
    enum tree_code op;
  } stack [num_precs];

  int sp = 0;
 
  tok = parser_get_token(parser);
  if (token_value(tok) == tv_lparen)
  {
    t = handle_cond_block(parser);
    parser_expect_tval(parser, tv_rparen);
    parser_get_token(parser);
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

    if (token_class(tok) == tok_operator)
    {
      switch (token_value (tok))
      {
        case tv_land:
          oprec = prec_logand;
          ocode = AND_EXPR;
          break;
        case tv_lor:
          oprec = prec_logor;
          ocode = OR_EXPR;
          break;
        default:
          parser_unget (parser);
          goto out;
      }

      while (oprec <= stack[sp].prec)
      {
        stack[sp-1].expr = make_binary_op ( stack[sp].op,
                                            stack[sp-1].expr,
                                            stack[sp].expr);
        sp--;
      }

      tok = parser_get_token (parser);
      if (token_value (tok) == tv_lparen)
      {
        t = handle_cond_block (parser);
        parser_expect_tval(parser, tv_rparen);
        parser_get_token(parser);
      }
      else
      {
        parser_unget (parser);
        t = handle_relations (parser);
      }

      if (t == NULL || t == error_mark_node)
      {
        while (sp >= 0)
        {
          free_tree(stack[sp--].expr);
          return error_mark_node;
        }
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
    stack[sp-1].expr = make_binary_op(  stack[sp].op, 
                                        stack[sp-1].expr,
                                        stack[sp].expr);
    sp--;
  }
  return stack[0].expr;
}

/* sexpr:
 * (sexpr) |
 * sexpr_op [ ( \land | \lor | \oplus | + | - | \cdot | divide | \ll | \gg |
 * \mod ) sexpr_op ]*
 */

tree 
handle_sexpr (struct parser * parser)
{
  tree t;
  struct token * tok;
  enum prec {
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

  struct {
    tree expr;
    enum prec prec;
    enum tree_code op;
  } stack [num_precs];

  int sp = 0;

  tok = parser_get_token(parser);
  if (token_value(tok) == tv_lparen)
  {
    t = handle_sexpr(parser);
    parser_expect_tval(parser, tv_rparen);
    parser_get_token(parser);
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

    if (token_class(tok) == tok_operator)
    {
      switch (token_value (tok))
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
          ocode = AND_EXPR;
          break;
        case tv_lor:
          oprec = prec_rel;
          ocode = OR_EXPR;
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
        stack[sp-1].expr = make_binary_op ( stack[sp].op,
                                            stack[sp-1].expr,
                                            stack[sp].expr);
        sp--;
      }

      tok = parser_get_token (parser);
      if (token_value (tok) == tv_lparen)
      {
        t = handle_sexpr (parser);
        parser_expect_tval(parser, tv_rparen);
        parser_get_token(parser);
      }
      else
      {
        parser_unget (parser);
        t = handle_sexpr_op (parser);
      }

      if (t == NULL || t == error_mark_node)
      {
        while (sp >= 0)
        {
          free_tree(stack[sp--].expr);
          return error_mark_node;
        }
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
    stack[sp-1].expr = make_binary_op(  stack[sp].op, 
                                        stack[sp-1].expr,
                                        stack[sp].expr);
    sp--;
  }
  return stack[0].expr;

}

/*
 * sexpr_op:
 * [ (\lnot | - ) ] ( <idx> | function_call )
 */

tree
handle_sexpr_op (struct parser * parser)
{
  tree t, t1;
  struct token * tok = parser_get_token (parser);
  bool prefix = false;

  if (token_value(tok) == tv_minus)
  {
    t = make_unary_op (UMINUS_EXPR,  NULL);
    prefix = true;
  }
  else if (token_value(tok) == tv_lnot)
  {
    t = make_unary_op (NOT_EXPR, NULL);
    prefix = true;
  }

  if (prefix)
    tok = parser_get_token (parser);

  if (token_class(tok) == tok_number || is_id (tok, false) || token_value(tok) == tv_frac || token_value(tok) == tv_dfrac)
  {
    parser_unget (parser);
    t1 = handle_idx(parser);
  }
  else if (token_value (tok) == tv_call)
  {
    parser_unget (parser);
    t1 = handle_function_call (parser);
  }
  else
  {
    error_loc (token_location (tok), "unexpected token `%s` ", token_as_string (tok));
    goto error;
  }

  if (prefix)
  {
    if (t1 != NULL && t1 != error_mark_node)
    {
      TREE_OPERAND_SET(t, 0, t1);
    }
    else
      goto error;
  }
  else
  {
    if (t1 != NULL && t1 != error_mark_node)
      t = t1;
    else 
      goto error;

  }
  return t;

error:
  free_tree(t1);
  free_tree(t);
  return error_mark_node;
}

/*
 * generator:
 * \forall <id> |
 * <id> [ , <id> ]* : sexpr [ comp sexpr ]+ [set_op sexpr [ comp sexpr ]+ ]*
 */
tree
handle_generator (struct parser* parser)
{
  struct token * tok = parser_get_token(parser);
  tree t, t1, ret;

  if(token_value(tok) == tv_forall)
  {
    t = handle_list(parser, handle_id, tv_comma);
    if (t == error_mark_node)
      goto shift;
    else
    {
      ret = make_tree (FORALL);
      TREE_OPERAND_SET(ret, 0, t);
      return ret;
    }
  }
  else
  {
    parser_unget (parser);
    t = handle_list(parser, handle_id, tv_comma); 
  
    if (t != error_mark_node)
    {
      ret = make_binary_op (GENERATOR, t, NULL);
  
    }
    else
      goto shift;

    if(!parser_forward_tval(parser,tv_colon))
      goto error;

    t1 = handle_cond_block(parser);

    if (t1 == NULL || t1 == error_mark_node)
      goto error;
  
    TREE_OPERAND_SET(ret, 1, t1);
    return ret;
  }
shift:
  parser_get_until_tval(parser, tv_colon);
error:
  free_tree(t);
  free_tree(t1);
  free_tree(ret);
  return error_mark_node;
}

/*
 * filter_op:
 * <id> ^ { [ <id> ] }
 */
tree
handle_filter_op (struct parser * parser)
{
  tree id = NULL, pow = NULL, ret = NULL;
  struct token * tok;
  if (!is_id ( tok = parser_get_token (parser), true))
    goto error;
  else
    id = make_identifier_tok (tok);

  if(!parser_forward_tval(parser, tv_circumflex))
    goto error;
  if(!parser_forward_tval(parser, tv_lbrace))
    goto error;
  if(!parser_forward_tval(parser, tv_lsquare))
    goto error;

  if (!is_id ( tok = parser_get_token (parser), true))
    goto error;
  else
    pow = make_identifier_tok (tok);

  if(!parser_forward_tval (parser, tv_rsquare))
    goto error;
  if(!(parser_forward_tval (parser, tv_rbrace)))
  {
    parser_unget (parser);  
    goto error;
  }

  ret = make_tree(CIRCUMFLEX);
  TREE_OPERAND_SET(ret, 0, id);
  TREE_OPERAND_SET(ret, 1, pow);
  ret->circumflex_node.is_index = true;
  return ret;

error:
  free_tree(id);
  free_tree(pow);
  free_tree(ret);
  parser_get_until_tval(parser, tv_rbrace);
  return error_mark_node;
}

/*
 * filter:
 * \filter { <id> ^ { [ <id> ] } [ , <id> ^ { [ <id> ] } ]* | generator }
 */
tree
handle_filter (struct parser * parser)
{
  tree ids = NULL, gen = NULL, ret = NULL;

  if (!(parser_forward_tval(parser, tv_filter)))
    goto error;
  
  if(!parser_forward_tval(parser, tv_lbrace))
    goto error;
  else
    ids = handle_list(parser, handle_filter_op, tv_comma);

  if(!parser_forward_tval(parser, tv_vertical))
    goto error;

  gen = handle_generator (parser);

  if (!parser_forward_tval(parser, tv_rbrace))
  {
    parser_unget(parser);
    goto error;
  }
  
  ret = make_binary_op (FILTER_EXPR, ids, gen);
  
  return ret;
error:
  parser_get_until_tval(parser, tv_rbrace);
  free_tree(ret);
  free_tree(ids);
  free_tree(gen);
  return error_mark_node;
}

/*
  matrix:
  \begin { tmatrix } { id }
    [ sexpr [ & sexpr ]* \lend ]+
  \end { tmatrix }
*/
tree
handle_matrix (struct parser * parser)
{
  tree format, list, t;
  struct token * tok;
  struct location loc;

  if(!(tok = parser_forward_tval (parser, tv_begin)))
    goto shift;
  else
    loc = token_location (tok);

  if(!parser_forward_tval (parser, tv_lbrace))
    goto shift;
  if(!parser_forward_tval (parser, tv_tmatrix))
    goto shift;
  if(!parser_forward_tval (parser, tv_rbrace))
    goto shift;
  if(!parser_forward_tval (parser, tv_lbrace))
    goto shift;

  if (!is_id ( parser_get_token (parser), true))
  {
    parser_unget (parser);
    goto shift;
  }
  parser_unget (parser);

  format = make_identifier_tok(parser_get_token(parser));

  if(!parser_expect_tval (parser, tv_rbrace))
    goto shift;


  list = make_tree_list();
  do
  {
    t = handle_list(parser, handle_sexpr, tv_delimiter);
    tree_list_append(list, t);
  } while(token_value(tok = parser_get_token (parser)) == tv_lend);
  
  if(token_value(tok) != tv_end)
    goto error;

  if(token_value(parser_get_token(parser)) != tv_lbrace)
    goto error;
  if(token_value(parser_get_token(parser)) != tv_tmatrix)
    goto error;
  if(token_value(parser_get_token(parser)) != tv_rbrace)
    goto error;

  return make_matrix (format, list, loc);

shift:
  while(true)
  {
    parser_get_until_tval(parser, tv_end);
    if(token_value(parser_get_token(parser)) != tv_lbrace)
      continue;
    if(token_value(parser_get_token(parser)) != tv_tmatrix)
      continue;
    if(token_value(parser_get_token(parser)) != tv_rbrace)
      continue;
    break;
  }
error:
  free_tree(list);
  free_tree(t);
  free_tree(format);
  return error_mark_node;
}

/*
  vector:
  \begin { tvector }
    [ sexpr \lendl ]+
  \end { tvector }
*/
tree
handle_vector (struct parser * parser)
{
  tree list, t;
  struct token * tok;
  struct location loc;

  if(!(tok = parser_forward_tval (parser, tv_begin)))
    goto shift;
  else
    loc = token_location (tok);

  if(!parser_forward_tval (parser, tv_lbrace))
    goto shift;
  if(!parser_forward_tval (parser, tv_tvector))
    goto shift;
  if(!parser_forward_tval (parser, tv_rbrace))
    goto shift;

  list = make_tree_list();
  do
  {
    t = handle_sexpr(parser);
    tree_list_append(list, t);
  } while(token_value(tok = parser_get_token (parser)) == tv_lend);
  
  if(token_value(tok) != tv_end)
    goto error;

  if(token_value(parser_get_token(parser)) != tv_lbrace)
    goto error;
  if(token_value(parser_get_token(parser)) != tv_tvector)
    goto error;
  if(token_value(parser_get_token(parser)) != tv_rbrace)
    goto error;

  return make_vector (list, loc);

shift:
  while(true)
  {
    parser_get_until_tval(parser, tv_end);
    if(token_value(parser_get_token(parser)) != tv_lbrace)
      continue;
    if(token_value(parser_get_token(parser)) != tv_tvector)
      continue;
    if(token_value(parser_get_token(parser)) != tv_rbrace)
      continue;
    break;
  }
error:
  free_tree(list);
  free_tree(t);
  return error_mark_node;
}

/*
  genarray:
  \genar \limits ^ { expr } ( sexpr )
*/
tree
handle_genarray (struct parser * parser)
{
  tree lim, exp;
  struct token * tok;
  struct location loc;

  if (!(tok = parser_forward_tval (parser, tv_genar)))
    goto shift;
  else
    loc = token_location(tok);
  if (!parser_forward_tval (parser, tv_limits))
    goto shift;
  if (!parser_forward_tval (parser, tv_circumflex))
    goto shift;
  if (!parser_forward_tval (parser, tv_lbrace))
    goto shift;
  
  lim = handle_sexpr (parser);
  
  if(!parser_forward_tval (parser, tv_rbrace))
    goto shift;
  if(!parser_forward_tval (parser, tv_lparen))
    goto shift;

  exp = handle_sexpr (parser);

  if(!parser_forward_tval (parser, tv_rparen))
    goto error;

  return make_genar(lim, exp, loc);
    
shift:
  parser_get_until_tval (parser, tv_rparen);
error:
  free_tree (lim);
  free_tree (exp);
  return error_mark_node;
}

/*
  expr:
  sexpr | filter | genarray | vector | matrix
*/
tree
handle_expr (struct parser * parser)
{
  struct token * tok;

  if (token_value (tok = parser_get_token (parser)) == tv_filter)
  {
    parser_unget (parser);
    return handle_filter (parser);
  }
  else if (token_value (tok) == tv_genar)
  {
    parser_unget (parser);
    return handle_genarray (parser);
  }
  else if (token_value (tok) == tv_begin)
  {
    if (token_value (tok = parser_get_token (parser)) == tv_lbrace)
    {
      if (token_value (tok = parser_get_token (parser)) == tv_tmatrix)
      {
        parser_unget (parser);
        parser_unget (parser);
        parser_unget (parser);
        return handle_matrix (parser);
      }
      else if (token_value (tok) == tv_tvector)
      {
        parser_unget (parser);
        parser_unget (parser);
        parser_unget (parser);
        return handle_vector (parser);
      }
      else
        parser_unget (parser);
    }
    else
      parser_unget (parser);
  }
  
  parser_unget (parser);
  return handle_sexpr (parser);
}

/*
  return:
  \return { expr }
*/
tree 
handle_return (struct parser * parser)
{
  tree t, ret;
  struct token * tok;
  struct location loc;

  if (token_value(tok = parser_get_token (parser)) != tv_return)
    goto shift;
  else
    loc = token_location (tok);

  if (!parser_forward_tval (parser, tv_lbrace))
    goto shift;

  t = handle_expr (parser);

  if (!parser_forward_tval (parser, tv_rbrace))
    goto error;

  ret =  make_return (t, loc);
  return ret;
shift:
  parser_get_until_tval (parser, tv_rbrace);
error:
  return error_mark_node;
}

/*
  assign:
  <id> \gets expr
*/
tree handle_assign (struct parser * parser)
{
  tree id = NULL, expr = NULL;

  if (!is_id ( parser_get_token (parser), true))
  {
    parser_unget (parser);
    goto error;
  }

  parser_unget (parser);

  id = make_identifier_tok (parser_get_token (parser));

  if (!parser_forward_tval (parser, tv_gets))
    goto error;
  
  expr = handle_expr (parser);
  
  return make_binary_op (ASSIGN_EXPR, id, expr);

error:
  free_tree (id);
  free_tree (expr);
  return error_mark_node;
}

/*
  declare:
  <id> \in expr
*/
tree handle_declare (struct parser * parser)
{
  tree id = NULL, type = NULL;
  
  if (!is_id ( parser_get_token (parser), true))
  {
    parser_unget (parser);
    goto error;
  }
  
  parser_unget (parser);

  id = make_identifier_tok (parser_get_token (parser));

  if (!parser_forward_tval (parser, tv_in))
    goto error;
  
  type = handle_ext_type (parser);
  return make_binary_op (DECLARE_EXPR, id, type);

error:
  free_tree (id);
  free_tree (type);
  return error_mark_node;
}

/*
  instr:
  assign | declare | return
*/
tree
handle_instr ( struct parser * parser)
{
  struct token * tok;
  
  if (token_value (tok = parser_get_token (parser)) == tv_return)
  {
    parser_unget (parser);
    return handle_return (parser);
  }
  else if ( is_id (tok, false))
  {
    if (token_value (tok = parser_get_token (parser)) == tv_gets)
    {
      parser_unget (parser);
      parser_unget (parser);
      return handle_assign (parser);
    }
    else if (token_value (tok) == tv_in)
    {
      parser_unget (parser);
      parser_unget (parser);
      return handle_declare (parser);
    }
    else if (token_value (tok) == tv_vertical)
    {
      parser_unget (parser);
      parser_unget (parser);
      return handle_with_loop (parser);
    }
    else 
      parser_unget (parser);
  }
  
  error_loc (token_location (tok), "unexpected token `%s` ", token_as_string (tok));
  return error_mark_node;
}

/*
  with_loop:
  idx | condition \gets ( expr | with_loop_cases )
*/
tree 
handle_with_loop (struct parser * parser)
{
  tree t = NULL, idx = NULL, cond = NULL, expr = NULL;
  
  idx = handle_idx (parser); 
  if (idx == NULL || idx == error_mark_node)
    goto error;

  if (!parser_forward_tval (parser, tv_vertical))
    goto error;

  cond = handle_generator (parser);
  if (cond == NULL || cond == error_mark_node)
    goto error;
  
  if (!parser_forward_tval (parser, tv_gets))
    goto error;

  if (token_value (parser_get_token (parser)) != tv_begin)
  {
    parser_unget (parser);
    expr = handle_expr (parser);
    if (cond == NULL || cond == error_mark_node)
      goto error;
    t = make_with_loop (idx, cond, expr, false);
  }
  else
  {
    parser_unget (parser);
    expr = handle_with_loop_cases (parser);
    t = make_with_loop (idx, cond, expr, true);
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
  with_loop_cases:
*/
tree
handle_with_loop_cases (struct parser * parser)
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

  list = make_tree_list();
  while (true)
  {
    expr = handle_expr (parser);
  
    if (expr == NULL || expr == error_mark_node)
      goto error;

    if (!parser_forward_tval (parser, tv_delimiter))
      goto error;
    
    if (token_value (parser_get_token (parser)) == tv_otherwise)
    {
      gen = make_tree (OTHERWISE_EXPR);
    
      if (token_value(parser_get_token (parser)) != tv_lend)
        parser_unget (parser);
      
      tree_list_append (list, make_binary_op (CASE_EXPR, expr, gen));
      break;
    }
    else
    {
      parser_unget (parser);
      gen = handle_generator(parser);

      if (!parser_forward_tval (parser, tv_lend))
        goto error;
      tree_list_append (list, make_binary_op (CASE_EXPR, expr, gen));
    }
  }
  
  if (is_end (parser, tv_cases))
  {
    return list;
  }

error:
  free_tree (list);
  free_tree (gen);
  free_tree (expr);
  return error_mark_node;
}

/* Top level function to parse the file.  */
int
parse (struct parser *parser)
{
  struct token *  tok;
  struct tree_list_element * tle;

  error_count = warning_count = 0;
  while (token_class (tok = parser_get_token (parser)) != tok_eof)
    {
      parser_unget(parser);
      tree t = handle_function(parser);
      
      if (t != NULL)
        tree_list_append(function_list, t);
    }
           
  printf ("note: finished parsing.\n");
  if (error_count != 0)
    {
      printf ("note: %i errors found.\n", error_count);
      return -3;
    }

  printf("\n######### Output ########\n");
  TAILQ_FOREACH (tle, &TREE_LIST_QUEUE(function_list), entries)
  {
    if (tle->element != error_mark_node)
    {
      print_expression(stdout, tle->element);
      if (TAILQ_NEXT (tle, entries))
        printf("\n");
    }
    else
      printf("Errors in function\n\n");
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
