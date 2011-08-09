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
struct token * parser_toke_alternative_tval(struct parser *, enum token_kind, enum token_kind);
struct token * parser_toke_alternative_tclass(struct parser *, enum token_class, enum token_class);
bool parser_expect_tval (struct parser *, enum token_kind);
bool parser_expect_tclass (struct parser *, enum token_class);
bool parser_init (struct parser *, struct lexer *);
bool parser_finalize (struct parser *);

tree handle_idx (struct parser *);
tree handle_idx_or_idx_list (struct parser *);
tree handle_function_call (struct parser *);
tree handle_upper (struct parser *);
tree handle_linear (struct parser *);
tree handle_sexpr_op (struct parser *);
tree handle_sexpr (struct parser *);
tree handle_lower (struct parser *);
tree handle_divide (struct parser *);

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
  
  if (token_class(tok) == tok_id) 
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

/*
 * Read idx or idx list
 * idx  [ , idx ]*
 */
tree handle_idx_or_idx_list (struct parser * parser)
{
  tree ret, idx;
  struct token * tok; 
  idx = handle_idx (parser);

  if(token_value(tok = parser_get_token(parser)) != tv_comma)
  {
    ret = idx;
  }
  else
  {
    tree idx_list = make_tree_list ();
    tree_list_append(idx_list, idx);
    while(true)
    {
      idx = handle_idx(parser);
      if (idx != NULL && idx != error_mark_node)
      {
        tree_list_append(idx_list, idx);
      }
      tok = parser_get_token(parser);
      if (token_value(tok) != tv_comma)
        break;

    }
  }
  parser_unget(parser);
  return ret;
}


/*
 * Read sexp or sexpr list
 * sexpr [ , sexpr ]*
 */
tree handle_sexpr_or_sexpr_list (struct parser * parser)
{
  tree ret, sexpr;
  struct token * tok;
  sexpr = handle_sexpr (parser);



  if(token_value(tok = parser_get_token(parser)) != tv_comma)
  {
    ret = sexpr;
  }
  else
  {
    tree sexpr_list = make_tree_list();
    tree_list_append (sexpr_list, sexpr);
    while (true)
    {
      sexpr = handle_sexpr(parser);

      if (sexpr != NULL && sexpr != error_mark_node)
      {
        tree_list_append(sexpr_list, sexpr);
      }
      tok = parser_get_token(parser);
      if (token_value(tok) != tv_comma)
        break;
    }
    ret = sexpr_list;
  }
  parser_unget(parser);

  return ret;
}

/*
 *  lower
 *  _ { sexpr [ , sexpr ]* }
 */
tree handle_lower (struct parser* parser)
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

/*
 * function_call:
 * \call { <id> } { [ idx [, idx ]* ] }
 */
tree handle_function_call (struct parser* parser)
{
  tree t = NULL, args = NULL;
  struct token * tok;

  if (!(tok = parser_forward_tval(parser, tv_call)))
    goto error;

  if (!(tok = parser_forward_tval(parser, tv_lbrace)))
    goto error;

  t = make_tree (FUNCTION_CALL);

  if (!(tok = parser_forward_tclass(parser, tok_id)))
    goto error;
  else
    TREE_OPERAND_SET(t, 0, make_identifier_tok (tok));

  if(!(tok = parser_forward_tval(parser, tv_rbrace)))
    goto error;
  if(!(tok = parser_forward_tval(parser, tv_lbrace)))
    goto error;
 
  TREE_OPERAND_SET(t, 1, NULL);
  tok = parser_get_token(parser);
  if(token_class(tok) == tok_id || token_class(tok) == tok_number)
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

  if (!(token_class(tok = parser_get_token (parser)) == tok_id))
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
tree handle_divide (struct parser * parser)
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

/* sexpr:
 * (sexpr) |
 * sexpr_op [ ( \land | \lor | \oplus | + | - | \cdot | divide | \ll | \gg |
 * \mod ) sexpr_op ]*
 */

tree handle_sexpr (struct parser * parser)
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


  if (token_class(tok) == tok_number || token_class(tok) == tok_id || token_value(tok) == tv_frac || token_value(tok) == tv_dfrac)
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

/* Top level function to parse the file.  */
int
parse (struct parser *parser)
{
  struct token *  tok;
  
  error_count = warning_count = 0;
  while (token_class (tok = parser_get_token (parser)) != tok_eof)
    {
      /*
      switch (token_class (tok))
        {

        case tok_keyword:
          switch (token_value (tok))
            {
              case tv_call:
                parser_unget (parser);
                handle_sexpr_op(parser);
                break;
            default:
              error_loc( token_location (tok), "keyword `%s` is not expected here",
                  token_as_string(tok));
            }
          break;
        case tok_id:
          //parser_unget (parser);
          //handle_idx_or_idx_list (parser);
          break;
        case tok_unknown:
          error_loc( token_location (tok), "unknown token found `%s`", 
              token_as_string(tok));
          break;
        default:
          error_loc (token_location (tok), "token `%s` is not expected here",
            token_as_string(tok));
        }
      */
      parser_unget(parser);
      print_expression(stdout, handle_sexpr(parser));
      printf("\n");
      //handle_sexpr (parser);
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
