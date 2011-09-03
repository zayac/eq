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
#include <ctype.h>
#include <err.h>

#include "expand.h"

#define TOKEN_KIND(a, b) b,
#define KEYWORD(a, b, c, d) d,
const char *token_kind_name[] = 
{
#include "token_kind.def"
#include "keywords.def"
};
#undef TOKEN_KIND
#undef KEYWORD

#define KEYWORD(a, b, c, d) tok_ ## b,
const enum token_class keyword_type[] = 
{
#include "keywords.def"
};
#undef KEYWORD

#define KEYWORD(a, b, c, d) c,
const bool is_token_id[] = 
{
#include "keywords.def"
};
#undef KEYWORD

#define TOKEN_CLASS(a, b) b,
const char *token_class_name[] =
{
#include "token_class.def"
};
#undef TOKEN_CLASS

/* This is a pointer to the first token from keywords.def  */
const char **  keywords = &token_kind_name[(int) tv_boolean];
size_t keywords_length = tok_kind_length  - tv_boolean;

/* Binary search function to search string in a char** table.  */
static inline size_t
kw_bsearch (const char *key, const char *table[], size_t len)
{
  size_t l = 0, r = len;

  while (l < r)
    {
      size_t hit = (l + r) / 2;
      int i = strcmp (key, table[hit]);
      /* printf ("%s ? %s, [%i, %i, %i]\n", key, table[hit], l, r, i); */

      if (i == 0)
	      return hit;
      else if (i < 0)
	      r = hit;
      else
	      l = hit + 1;
    }
  return len;
}

/* Initialize lexer LEX with a file name FNAME and
   set initial parameters of the lexer.  */
bool
lexer_init (struct lexer *lex, const char *fname)
{
  assert (fname != NULL, "lexer initialized with empty filename");
  assert (lex != NULL, "lexer memory is not allocated");
  
  lex->hex_number = false;
  lex->is_eof = false;
  lex->loc = (struct location) {1, 0};
  lex->fname = fname;
  lex->file = fopen (fname, "r");

  if (!lex->file)
    {
      warn ("error opening file `%s'", fname);
      return false;
    }
  
  /* tval_intit (&(lex->cur_token), tok_eof, tv_eof); */
  return true;
}

/* Actions before deallocating lexer.  */
bool
lexer_finalize (struct lexer *lex)
{
  fclose (lex->file);
  return true;
}


/* Gets one character from the file, is end of file is
   reached, it will return EOF in all the consequent calls.  */
static inline char
lexer_getch (struct lexer *lex)
{
  int ch;
  
  if (lex->is_eof)
    return EOF;

  ch = fgetc (lex->file);
  if (ch == EOF)
    {
      lex->is_eof = true;
      return EOF;
    }

  if (ch == '\n')
    {
      lex->loc.line++;
      lex->loc.col = 0;
    }
  else
    lex->loc.col++;
  return (char)ch;
}

/* Put character back on the stream of the lexer.
   Consequent lexer_getch should return exactly this character.  */
static inline void
lexer_ungetch (struct lexer *lex, char ch)
{
  if (ch == '\n')
    lex->loc.line--;
  /* FIXME position should show the last symbol
           of previous line, not -1.  */
  lex->loc.col--;
  ungetc (ch, lex->file);
}

/* Adds the character C to the string *BUFFER that has length *SIZE
   at the position *INDEX. *INDEX is a pointer in the *BUFFER.
   If the *BUFFER is NULL then it is being allocated, if the *INDEX
   points at the end of the *BUFFER the *BUFFER will be reallocated. */
static inline void
buffer_add_char (char **buffer, char **index, size_t *size, char c)
{
  const size_t initial_size = 16;

  if (*buffer == NULL)
    {
      *buffer = (char *) malloc (initial_size *sizeof(char));
      *index = *buffer;
      *(*index)++ = c;
      *size = initial_size;
      return;
    }

  assert (*index <= *buffer + *size, 
          "index is greater than allocated buffer");
  
  if (*index == *buffer + *size)
    {
      *buffer = (char *) realloc (*buffer, *size * 2 * sizeof (char));
      *index = *buffer +*size;
      *size *= 2;
    }
    
  *(*index)++ = c;
}

/* Internal function to read until the end of comment.  */
static inline enum token_class
lexer_read_comments (struct lexer *lex, char **buf, size_t *size)
{
  char *index = *buf;

  buffer_add_char (buf, &index, size, '%');

  while (true)
    {
      char c = lexer_getch (lex);
      
      if (c == EOF)
        break;
	
      buffer_add_char (buf, &index, size, c);
      if (c == '\n')
        break;
    }

   buffer_add_char (buf, &index, size, '\0');
   return tok_comments;
}

/* Function to read a hex number */
static inline void
lexer_read_hex_number (struct lexer *lex, struct token* tok,
			  char **buf, size_t *size, char c)
{
  char *index = *buf;
  do
    {
      buffer_add_char (buf, &index, size, c);
      c = lexer_getch (lex);
    }
  while (isxdigit (c));

  lexer_ungetch (lex, c);
  buffer_add_char(buf, &index, size, 0);
  tok->tok_class = tok_number;
  lex->hex_number = false;
}

/* Internal function to read a string,
   checking if it is a keyword, an operator or id */
static inline void
lexer_read_keyword (struct lexer *lex, struct token *tok,
                        char **buf, size_t *size, char c)
{
  char *index = *buf;
  size_t search;
  bool first = true;

  do
    {
      if (c == '\\' && !first)
	{
	  c = lexer_getch (lex);
	  if (c != '_')
	    {
	      lexer_ungetch (lex, c);
	      break;
	    }
	}
      buffer_add_char (buf, &index, size, c);
      c = lexer_getch (lex);
      first = false;
    }
  while (isalnum (c) || (c == '\\'));
  lexer_ungetch (lex, c);
  buffer_add_char(buf, &index, size, 0);

  search = kw_bsearch (*buf, keywords, keywords_length);

  if (search != keywords_length)
    {
      if (*buf)
        free (*buf);
      *size = 0;
      *buf = NULL;
      tval_tok_init (tok, keyword_type[search], (enum token_kind)(search + tv_boolean));
      return;
    }
 
  if (**buf != '\\')
    tok->tok_class = tok_id;
  else
    {
      tok->uses_buf = true;
      tok->tok_class = tok_unknown;
    }
}

/* Internal function to read until the end of number.  */
static inline enum token_class
lexer_read_number (struct lexer *lex, char **buf, size_t *size, char c)
{
  //bool ishex = false;
  char *index = *buf;

  buffer_add_char (buf, &index, size, c);
  /*if (c == '0')
    {
      c = lexer_getch (lex);

      if (c == 'x' || c == 'X')
        {
          ishex = true;
          buffer_add_char (buf, &index, size, c);
        }
        else
          lexer_ungetch(lex, c);
    }
  */

  /* middle of the number */

  while (true)
    {
      c = lexer_getch (lex);
      if (isdigit (c)
              /* ||  (ishex && ((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')))*/)
        buffer_add_char (buf, &index, size, c);
      else
        {
          lexer_ungetch(lex, c);
          break;
        }
    }

  c = lexer_getch(lex);

  if (c == '.')
    {
      /*if (ishex)
        {
          error_loc(lex->loc, "\'%c\' found in hex number", c);
          return tok_unknown;
        }
      */
      buffer_add_char(buf, &index, size, c);
      
      c = lexer_getch (lex);
      buffer_add_char(buf, &index, size, c);
      if (!isdigit(c))
        {
          error_loc(lex->loc, "there should be at least one digit after dot. \'%c\' found", c);
          return tok_unknown;
        }

      while((c = lexer_getch (lex)) && isdigit(c))
        buffer_add_char(buf, &index, size, c);

    }

  if (c == 'e' || c == 'E')
    {
      buffer_add_char(buf, &index, size, c);

      c = lexer_getch(lex);
      if(isdigit(c))
        buffer_add_char(buf, &index, size, c);
      else
        {
          error_loc(lex->loc, "there should be at least one digit after \'E\'. \'%c\' found", c);
          return tok_unknown;
        }

      while((c = lexer_getch (lex)) && isdigit(c))
        buffer_add_char(buf, &index, size, c);
    }
  lexer_ungetch (lex, c);
  buffer_add_char (buf, &index, size, 0);

  return tok_number;
}

/* Reads the stream from lexer and returns dynamically allocated token
   of the appropriate type.  */
struct token *
lexer_get_token (struct lexer *lex)
{
  char c;
  struct location loc = lex->loc;
  struct token *tok = (struct token *) malloc (sizeof (struct token));
  size_t buf_size=16;
  char *buf = NULL;


  c = lexer_getch (lex);
  if (isspace (c))
    {
      while (EOF != (c = lexer_getch (lex)) && isspace (c))
        ;
      loc = lex->loc;
    }

  if (c == EOF)
    {
      tval_tok_init (tok, tok_eof, tv_eof);
      goto return_token;
    }

  if (c == '%')
    {
      tok->tok_class = lexer_read_comments(lex, &buf, &buf_size);
      goto return_token;

    }

  if(c == '/')
    {
      char c1 = lexer_getch(lex);
      if (c1 == '/')
        {
          tval_tok_init( tok, tok_keyword, tv_lend);
          goto return_token;
        }
      else
        lexer_ungetch(lex, c1);
    }

  if(c == '\\')
    {
      char c1 = lexer_getch(lex);
      switch(c1)
      {
        case ',':
          tval_tok_init (tok, tok_whitespace, tv_small_space); goto return_token;       
        case ':':
          tval_tok_init (tok, tok_whitespace, tv_medium_space); goto return_token;     
        case ';':
          tval_tok_init (tok, tok_whitespace, tv_large_space); goto return_token;     
        case ' ':
          tval_tok_init (tok, tok_whitespace, tv_space); goto return_token;     
        default:
          lexer_ungetch(lex, c1);
          lexer_read_keyword(lex, tok, &buf, &buf_size, c);
          goto return_token;
      }
    }

  if (isalpha (c))
    {
      if (isxdigit(c) && lex->hex_number)
	lexer_read_hex_number (lex, tok, &buf, &buf_size, c);
      else
	lexer_read_keyword(lex, tok, &buf, &buf_size, c);
      goto return_token;
    }

  if (isdigit (c))
    {
      if (lex->hex_number)
	lexer_read_hex_number (lex, tok, &buf, &buf_size, c);
      else
	tok->tok_class = lexer_read_number (lex, &buf, &buf_size, c);
      goto return_token;
    }

  if (c == '&')
    {
      tval_tok_init (tok, tok_operator, tv_delimiter);
      goto return_token;
    }
  
  if (c == '+')
    {
      tval_tok_init (tok, tok_operator, tv_plus);
      goto return_token;
    }
   
  if (c == '|')
    {
      tval_tok_init (tok, tok_operator, tv_vertical);
      goto return_token;
    }
  if (c == '-')
    {
      tval_tok_init (tok, tok_operator, tv_minus);
      goto return_token;
    } 
  
  if (c == '=')
    {
      tval_tok_init (tok, tok_operator, tv_eq);
      goto return_token;
    }
  
  if (c == '^')
    {
      tval_tok_init (tok, tok_operator, tv_circumflex);
      goto return_token;
    }

  if (c == '_')
    {
      tval_tok_init (tok, tok_operator, tv_lower_index);
      goto return_token;
    }


  if (c == '>')
    {
      tval_tok_init (tok, tok_operator, tv_gt);
      goto return_token;
    }
  
  if (c == '<')
    {
      tval_tok_init (tok, tok_operator, tv_lt);
      goto return_token;
    }

  switch (c)
    {
    case ',': 
      tval_tok_init (tok, tok_operator, tv_comma); goto return_token;
    case '(': 
      tval_tok_init (tok, tok_operator, tv_lparen); goto return_token;
    case ')': 
      tval_tok_init (tok, tok_operator, tv_rparen); goto return_token;
    case '[': 
      tval_tok_init (tok, tok_operator, tv_lsquare); goto return_token;
    case ']': 
      tval_tok_init (tok, tok_operator, tv_rsquare); goto return_token;
    case '{': 
      tval_tok_init (tok, tok_operator, tv_lbrace); goto return_token;
    case '}': 
      tval_tok_init (tok, tok_operator, tv_rbrace); goto return_token;
    case ':': 
      tval_tok_init (tok, tok_operator, tv_colon); goto return_token;
    default:
      ;
    }


  /* if nothing was found, we construct an unknown token  */
  assert (buf == NULL, "buf was used, but token_class is missing");
  buf = (char *) malloc (2 * sizeof (char));
  buf[0] = c; buf[1] = 0;
  tok->tok_class = tok_unknown;

return_token:
  assert (tok->tok_class >= tok_keyword && tok->tok_class <= tok_unknown,
          "token type was not provided");
  
  if (buf != NULL)
    tok->value.cval = buf;
  
  tok->loc = loc;
  return tok;
}


/* If the value of the token needs a character buffer or it is
   stored as an enum token_kind variable.  */
inline bool
token_uses_buf (enum token_class tclass)
{
  switch (tclass)
    {
    case tok_id:
    case tok_number:
    case tok_comments:
    case tok_unknown:
      return true;
    default:
      return false;
    }
}

/* String representation of the token TOK.  */
const char *
token_as_string (struct token * tok)
{
  
  if (token_uses_buf (token_class (tok)))
    return tok->value.cval;
  else
    return token_kind_name [(int) tok->value.tval];
}


/* Prints the token.  */
void
token_print (struct token *tok)
{
  const char *tokval = token_as_string (tok);

  (void) fprintf (stdout, "%d:%d %s ", (int)tok->loc.line, 
                  (int)tok->loc.col, token_class_name[(int) tok->tok_class]);

  if (tok->tok_class != tok_unknown)
    (void) fprintf (stdout, "['%s']\n", tokval);
  else
    (void) fprintf (stdout, "['%s'] !unknown\n", tokval);
    
  fflush (stdout);
}

/* Copy token. Also copies string if necessary.
   Memory allocation is done too.
 */
struct token* 
token_copy (struct token *tok)
{
  struct token * ret;
  if (tok == NULL)
    return NULL;

  ret = (struct token *) malloc (sizeof (struct token));
  ret->loc = tok->loc;
  ret->tok_class = tok->tok_class;
  ret->uses_buf = tok->uses_buf;
  if (token_uses_buf (token_class (tok)))
    ret->value.cval = strdup (tok->value.cval);
  else
    ret->value.tval = tok->value.tval;
  return ret;
}

/* Compare two tokens 
   It doesn't take into consideration token locations
 */
int
token_compare (struct token * first, struct token * second)
{
  if (first == second)
    return 0;
  
  /* Compare token classes  */
  if (first->tok_class < second->tok_class)
    return -1;
  else if (first->tok_class > second->tok_class)
    return 1;

  /* Compare by buffer usage  */
  if (first->uses_buf != second->uses_buf)
    {
      if (!first->uses_buf)
	return -1;
      else
	return 1;
    }

  if(first->uses_buf)
      return strcmp (first->value.cval, second->value.cval);
  else
    {
      if (first->value.tval < second->value.tval)
	return -1;
      else if (first->value.tval > second->value.tval)
	return 1;
      else return 0;
    }
  return 0;
}


/* Deallocates the memory that token occupies.  */
void
token_free (struct token *tok)
{
  assert (tok, "attempt to free NULL token");

  if (token_uses_buf (token_class (tok)) && tok->value.cval)
    free (tok->value.cval);
  free (tok);
  tok = NULL;
}


/* Main function if you want to test lexer part only.  */
#ifdef LEXER_BINARY
int error_count = 0;
int warning_count = 0;

int
main (int argc, char *argv[])
{
  struct lexer *lex = (struct lexer *) malloc (sizeof (struct lexer));
  struct token *tok = NULL;
  
  if (argc <= 1)
    {
      fprintf (stderr, "No input file\n");
      goto cleanup;
    }
  
  if (!lexer_init (lex, argv[1]))
    goto cleanup;

  while ((tok = lexer_get_token (lex))->tok_class != tok_eof)
    {
      token_print (tok);
      token_free (tok);     
    }

  token_free (tok);
  lexer_finalize (lex);

cleanup:
  if (lex)
    free (lex);

  return 0;
}
#endif
