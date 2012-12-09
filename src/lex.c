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
#include <ctype.h>
#include <err.h>

#include "eq.h"

#define TOKEN_KIND(a, b) b,
#define KEYWORD(a, b, c, d) d,
const char *eq_token_kind_name[] = {
#include "token_kind.def"
#include "keywords.def"
};

#undef TOKEN_KIND
#undef KEYWORD

#define KEYWORD(a, b, c, d) tok_ ## b,
const enum eq_token_class eq_keyword_type[] =
{
#include "keywords.def"
};

#undef KEYWORD

#define KEYWORD(a, b, c, d) c,
const bool eq_is_token_id[] = {
#include "keywords.def"
};

#undef KEYWORD

#define TOKEN_CLASS(a, b) b,
const char *eq_token_class_name[] = {
#include "token_class.def"
};

#undef TOKEN_CLASS

#define DELIMITER(a) a,
const char *eq_token_delimiters[] = {
#include "delimiters.def"
};

#undef DELIMITER


/* This is a pointer to the first token from keywords.def  */
const char **eq_keywords = &eq_token_kind_name[(int) tv_boolean];
size_t eq_keywords_length = eq_tok_kind_length - tv_boolean;


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
eq_lexer_init (struct eq_lexer * lex, const char *fname)
{
  FILE *f = fopen (fname, "r");

  if (!f)
    {
      warn ("error opening file `%s'", fname);
      return false;
    }

  return eq_lexer_init_file (lex,  f, fname);
}

/* Initialize lexer LEX with a file FILE, which is open
   by external program and the name FNAME that matches
   FILE.  Set initial parameters of the lexer.  */
bool
eq_lexer_init_file (struct eq_lexer * lex, FILE * f, const char *fname)
{
  assert (fname != NULL, "lexer initialized with empty filename");
  assert (lex != NULL, "lexer memory is not allocated");
  assert (f != NULL, "invalid file passed to lexer");

  lex->hex_number = false;
  lex->is_eof = false;
  lex->loc = (struct eq_location){1, 0};
  lex->fname = fname;
  lex->file = f;
  lex->error_notifications = false;
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
eq_lexer_finalize (struct eq_lexer * lex)
{
  fclose (lex->file);
  return true;
}


/* Gets one character from the file, is end of file is
   reached, it will return EOF in all the consequent calls.  */
static inline char
lexer_getch (struct eq_lexer *lex)
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
  return (char) ch;
}

/* Put character back on the stream of the lexer.
   Consequent lexer_getch should return exactly this character.  */
static inline void
lexer_ungetch (struct eq_lexer *lex, char ch)
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
buffer_add_char (char **buffer, char **index, size_t * size, char c)
{
  const size_t initial_size = 16;

  if (*buffer == NULL)
    {
      *buffer = (char *) malloc (initial_size * sizeof (char));
      memset(*buffer, 0, initial_size * sizeof (char));
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
      *index = *buffer + *size;
      *size *= 2;
    }

  *(*index)++ = c;
}

/* Internal function to read until the end of comment.  */
static inline enum eq_token_class
lexer_read_comments (struct eq_lexer *lex, char **buf, size_t * size)
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

/* Internal function to read until the end of string/char ignoring
escape sequences. */
static inline enum eq_token_class
lexer_read_string (struct eq_lexer *lex, char **buf, size_t * size, char c)
{
  char *index = *buf;
  const char stop = c;

  assert (stop == '"', "inapproriate starting symbol for string or char");

  buffer_add_char (buf, &index, size, stop);

  while (true)
    {
      c = lexer_getch (lex);
      if (c == EOF)
	{
	  if (lex->error_notifications)
	    error_loc (lex->loc,
		       "unexpected end of file in the middle of string");
	  buffer_add_char (buf, &index, size, 0);
	  return tok_unknown;
	}

      buffer_add_char (buf, &index, size, c);
      if (c == '\\')
	{
	  char cc = lexer_getch (lex);
	  if (cc == EOF)
	    {
	      if (lex->error_notifications)
		error_loc (lex->loc,
			   "unexpected end of file in the middle of string");
	      buffer_add_char (buf, &index, size, 0);
	      return tok_unknown;
	    }
	  buffer_add_char (buf, &index, size, cc);
	}
      else if (c == stop)
	break;
    }

  buffer_add_char (buf, &index, size, 0);
  return tok_string;
}

/* Function to read a hex number */
static inline void
lexer_read_hex_number (struct eq_lexer *lex, struct eq_token *tok, char **buf,
		       size_t * size, char c)
{
  char *index = *buf;
  do
    {
      buffer_add_char (buf, &index, size, c);
      c = lexer_getch (lex);
    }
  while (isxdigit (c));

  lexer_ungetch (lex, c);
  buffer_add_char (buf, &index, size, 0);
  tok->tok_class = tok_intnum;
  tok->uses_buf = true;
  lex->hex_number = false;
}

/* Internal function to read a string,
   checking if it is a keyword, an operator or id */
static inline void
lexer_read_keyword (struct eq_lexer *lex, struct eq_token *tok, char **buf,
		    size_t * size, char c)
{
  char *index = *buf;
  size_t search;
  bool first = true;

  do
    {
      if (c == '\\' && !first)
	{
	  char c1 = c;
	  c = lexer_getch (lex);
	  if (c != '_')
	    {
	      lexer_ungetch (lex, c);
	      c = c1;
	      break;
	    }
	}
      buffer_add_char (buf, &index, size, c);
      c = lexer_getch (lex);
      first = false;
    }
  while (isalnum (c) || (c == '\\'));
  lexer_ungetch (lex, c);
  buffer_add_char (buf, &index, size, 0);

  search = kw_bsearch (*buf, eq_keywords, eq_keywords_length);

  if (search != eq_keywords_length)
    {
      if (*buf)
	free (*buf);
      *size = 0;
      *buf = NULL;
      tval_tok_init (tok, eq_keyword_type[search],
		     (enum eq_token_kind) (search + tv_boolean));
      return;
    }

  if (**buf != '\\')
    {
      tok->tok_class = tok_id;
      tok->uses_buf = true;
    }
  else
    {
      tok->uses_buf = true;
      tok->tok_class = tok_unknown;
    }
}

/* Internal function to read until the end of number.  */
static inline enum eq_token_class
lexer_read_number (struct eq_lexer *lex, char **buf, size_t * size, char c)
{
  bool isreal = false;
  bool saw_dot = false;
  bool saw_exp = false;
  char *index = *buf;

  buffer_add_char (buf, &index, size, c);

  if (c == '.')
    {
      isreal = true;
      saw_dot = true;

      c = lexer_getch (lex);

      if (!isdigit (c))
	{
	  if (lex->error_notifications)
	    error_loc (lex->loc, "digit expected, '%c' found instead", c);
	  lexer_ungetch (lex, c);
	  goto return_unknown;
	}
      else
	buffer_add_char (buf, &index, size, c);
    }

  while (true)
    {
      c = lexer_getch (lex);
      if (c == EOF)
	{
	  if (lex->error_notifications)
	    error_loc (lex->loc, "unexpected end of file");
	  goto return_unknown;
	}
      else if (c == 'e' || c == 'E')
	{
	  if (saw_exp)
	    {
	      if (lex->error_notifications)
		error_loc (lex->loc, "exponent is specified more than once");
	      goto return_unknown;
	    }
	  isreal = true;

	  buffer_add_char (buf, &index, size, c);
	  c = lexer_getch (lex);

	  if (c == '+' || c == '-')
	    {
	      buffer_add_char (buf, &index, size, c);
	      c = lexer_getch (lex);
	    }

	  if (!isdigit (c))
	    {
	      if (lex->error_notifications)
		error_loc (lex->loc, "digit expected after exponent sign");
	      goto return_unknown;
	    }
	  else
	    buffer_add_char (buf, &index, size, c);

	  while (isdigit (c = lexer_getch (lex)))
	    buffer_add_char (buf, &index, size, c);

	  break;
	}
      else if (c == '.')
	{
	  if (saw_dot)
	    {
	      if (lex->error_notifications)
		error_loc (lex->loc, "more than one dot in the number ");
	      goto return_unknown;
	    }
	  saw_dot = true;
	  isreal = true;
	}
      else if (!isdigit (c))
	break;

      buffer_add_char (buf, &index, size, c);
    }
  lexer_ungetch (lex, c);
  buffer_add_char (buf, &index, size, 0);

  if (isreal)
    return tok_realnum;
  else
    return tok_intnum;

return_unknown:
  buffer_add_char (buf, &index, size, 0);
  return tok_unknown;
}

/* Reads the stream from lexer and returns dynamically allocated token
   of the appropriate type.  */
struct eq_token *
eq_lexer_get_token (struct eq_lexer *lex)
{
  char c;
  struct eq_location loc;
  struct eq_token *tok = (struct eq_token *) malloc (sizeof (struct eq_token));
  tok->uses_buf = false;
  size_t buf_size = 16;
  char *buf = NULL;

  c = lexer_getch (lex);
  loc = lex->loc;
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
      tok->tok_class = lexer_read_comments (lex, &buf, &buf_size);
      goto return_token;

    }

  if (c == '/')
    {
      char c1 = lexer_getch (lex);
      if (c1 == '/')
	{
	  tval_tok_init (tok, tok_keyword, tv_lend);
	  goto return_token;
	}
      else
	lexer_ungetch (lex, c1);
    }

  if (c == '"')
    {
      tok->tok_class = lexer_read_string (lex, &buf, &buf_size, c);
      goto return_token;
    }

  if (c == '\\')
    {
      char c1 = lexer_getch (lex);
      switch (c1)
	{
	case ',':
	  tval_tok_init (tok, tok_whitespace, tv_small_space);
	  goto return_token;
	case ':':
	  tval_tok_init (tok, tok_whitespace, tv_medium_space);
	  goto return_token;
	case ';':
	  tval_tok_init (tok, tok_whitespace, tv_large_space);
	  goto return_token;
	case ' ':
	  tval_tok_init (tok, tok_whitespace, tv_space);
	  goto return_token;
	default:
	  lexer_ungetch (lex, c1);
	  lexer_read_keyword (lex, tok, &buf, &buf_size, c);
	  goto return_token;
	}
    }

  if (isalpha (c))
    {
      if (isxdigit (c) && lex->hex_number)
	lexer_read_hex_number (lex, tok, &buf, &buf_size, c);
      else
	lexer_read_keyword (lex, tok, &buf, &buf_size, c);
      goto return_token;
    }

  if (c == '.')
    {
      tok->tok_class = lexer_read_number (lex, &buf, &buf_size, c);
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
      tval_tok_init (tok, tok_operator, tv_comma);
      goto return_token;
    case '(':
      tval_tok_init (tok, tok_operator, tv_lparen);
      goto return_token;
    case ')':
      tval_tok_init (tok, tok_operator, tv_rparen);
      goto return_token;
    case '[':
      tval_tok_init (tok, tok_operator, tv_lsquare);
      goto return_token;
    case ']':
      tval_tok_init (tok, tok_operator, tv_rsquare);
      goto return_token;
    case '{':
      tval_tok_init (tok, tok_operator, tv_lbrace);
      goto return_token;
    case '}':
      tval_tok_init (tok, tok_operator, tv_rbrace);
      goto return_token;
    case ':':
      tval_tok_init (tok, tok_operator, tv_colon);
      goto return_token;
    case ';':
      tval_tok_init (tok, tok_operator, tv_semicolon);
      goto return_token;
    default:
      ;
    }


  /* if nothing was found, we construct an unknown token.  */
  assert (buf == NULL, "buf was used, but token_class is missing");
  buf = (char *) malloc (2 * sizeof (char));
  buf[0] = c;
  buf[1] = 0;
  tok->tok_class = tok_unknown;

return_token:
  /* All tokens are valid except `tok_class_length'.  */
  assert (tok->tok_class <= tok_unknown, "token type was not provided");

  if (buf != NULL)
    {
      tok->uses_buf = true;
      tok->value.cval = buf;
    }

  tok->loc = loc;
  return tok;
}


/* If the value of the token needs a character buffer or it is
   stored as an enum token_kind variable.  */
inline bool
eq_token_uses_buf (struct eq_token * tok)
{
  return tok->uses_buf;
}

/* String representation of the token TOK.  */
const char *
eq_token_as_string (struct eq_token *tok)
{

  if (eq_token_uses_buf (tok))
    return tok->value.cval;
  else
    return eq_token_kind_name[(int) tok->value.tval];
}


/* Prints the token.  */
void
eq_token_print (struct eq_token *tok)
{
  const char *tokval = eq_token_as_string (tok);

  (void) fprintf (stdout, "%d:%d %s ", (int) tok->loc.line,
		  (int) tok->loc.col, eq_token_class_name[(int) tok->tok_class]);

  if (tok->tok_class != tok_unknown)
    (void) fprintf (stdout, "['%s']\n", tokval);
  else
    (void) fprintf (stdout, "['%s'] !unknown\n", tokval);

  fflush (stdout);
}

/* Copy token. Also copies string if necessary.
   Memory allocation is done too.
 */
struct eq_token *
eq_token_copy (struct eq_token *tok)
{
  struct eq_token *ret;
  if (tok == NULL)
    return NULL;

  ret = (struct eq_token *) malloc (sizeof (struct eq_token));
  ret->loc = tok->loc;
  ret->tok_class = tok->tok_class;
  ret->uses_buf = eq_token_uses_buf (tok);
  if (eq_token_uses_buf (tok))
    ret->value.cval = strdup (tok->value.cval);
  else
    ret->value.tval = tok->value.tval;
  return ret;
}

/* Compare two tokens
   It doesn't take into consideration token locations
 */
int
eq_token_compare (struct eq_token *first, struct eq_token *second)
{
  if (first == second)
    return 0;

  /* Compare token classes  */
  if (first->tok_class < second->tok_class)
    return -1;
  else if (first->tok_class > second->tok_class)
    return 1;

  /* Compare by buffer usage  */
  if (eq_token_uses_buf (first) != eq_token_uses_buf (second))
    {
      if (!eq_token_uses_buf (first))
	return -1;
      else
	return 1;
    }

  if (eq_token_uses_buf (first))
    return strcmp (first->value.cval, second->value.cval);
  else
    {
      if (first->value.tval < second->value.tval)
	return -1;
      else if (first->value.tval > second->value.tval)
	return 1;
      else
	return 0;
    }
  return 0;
}

/* Is needed to concatenate a possible delimiter with \left or \right token.  */
bool
eq_token_is_delimiter (struct eq_token * tok)
{
  size_t delimiters_length = sizeof (eq_token_delimiters) / sizeof (char *);
  size_t ret = kw_bsearch (eq_token_as_string (tok), eq_token_delimiters,
			   delimiters_length);
  return ret != delimiters_length;

}

/* Deallocates the memory that token occupies.  */
void
eq_token_free (struct eq_token *tok)
{
  assert (tok, "attempt to free NULL token");

  if (eq_token_uses_buf (tok) && tok->value.cval)
    free (tok->value.cval);
  free (tok);
  tok = NULL;
}


/* Main function if you want to test lexer part only.  */
#ifdef LEXER_BINARY
int eq_error_count = 0;
int eq_warning_count = 0;

int
main (int argc, char *argv[])
{
  struct eq_lexer *lex = (struct eq_lexer *) malloc (sizeof (struct eq_lexer));
  struct eq_token *tok = NULL;

  if (argc <= 1)
    {
      fprintf (stderr, "No input file\n");
      goto cleanup;
    }

  if (!eq_lexer_init (lex, argv[1]))
    goto cleanup;

  while ((tok = eq_lexer_get_token (lex))->tok_class != tok_eof)
    {
      eq_token_print (tok);
      eq_token_free (tok);
    }

  eq_token_free (tok);
  eq_lexer_finalize (lex);

cleanup:
  if (lex)
    free (lex);

  return 0;
}
#endif
