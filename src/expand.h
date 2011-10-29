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

#ifndef __EXPAND_H__
#   define __EXPAND_H__

#   include <stdio.h>
#   include <stdlib.h>
#   include <string.h>
#   include <stdarg.h>
#   include <unistd.h>

#   ifndef __cplusplus
typedef unsigned char bool;
#      define true 1
#      define false 0
#   endif

#   ifdef __cplusplus
#      include <cstdio>
#   endif

#   define LEXER_BUFFER  8192

static inline int
xfprintf (FILE * f, const char *fmt, ...)
{
  va_list args;

  if (fmt == 0 || strlen (fmt) == 0)
    return 0;
  else
    {
      va_start (args, fmt);
      vfprintf (f, fmt, args);
      va_end (args);
      return fprintf (f, "\n");
    }
}


extern int error_count;
extern int warning_count;

#   define assert(expr, ...) \
  ((expr) ? (void)0 \
                : (void)(fprintf (stderr, "%s:%i %s: Assertion (" \
                                     # expr ") failed.\n", \
                                     __FILE__, __LINE__, __func__), \
                    xfprintf (stderr, __VA_ARGS__), \
                    abort ()))

#   define unreachable(...)  \
  ((void) (fprintf (stderr, "Code in %s:%d reached impossible state.\n", \
                    __FILE__, __LINE__), \
           xfprintf (stderr, __VA_ARGS__), \
           abort ()))

#   define error_loc(loc, ...) \
  do {  \
    (void) fprintf (stderr, "error:%d:%d: ", (int)loc.line, (int)loc.col); \
    (void) fprintf (stderr, "[line=%i]  ", __LINE__); \
    (void) xfprintf (stderr, __VA_ARGS__); \
    ++error_count; \
  } while (0)

#   define error( ...) \
  do {  \
    (void) fprintf (stderr, "error: "); \
    (void) xfprintf (stderr, __VA_ARGS__); \
    ++error_count; \
  } while (0)

#   define warning_loc(loc, ...) \
  do {  \
    (void) fprintf (stderr, "warning:%d:%d: ", (int)loc.line, (int)loc.col); \
    (void) xfprintf (stderr, __VA_ARGS__); \
    ++ warning_count; \
  } while (0)

#   define warning(...) \
  do {  \
    (void) fprintf (stderr, "warning: "); \
    (void) xfprintf (stderr, __VA_ARGS__); \
    ++ warning_count; \
  } while (0)

#   define TOKEN_KIND(a, b) a,
#   define KEYWORD(a, b, c, d) tv_ ## a,
enum token_kind
{
#   include "token_kind.def"
#   include "keywords.def"
  tok_kind_length
};
#   undef TOKEN_KIND
#   undef KEYWORD

#   define TOKEN_CLASS(a, b) tok_ ## a,
enum token_class
{
#   include "token_class.def"
  tok_class_length
};
#   undef TOKEN_CLASS

struct location
{
  size_t line, col;
};

struct token
{
  struct location loc;
  enum token_class tok_class;
  bool uses_buf;
  union
  {
    char *cval;
    enum token_kind tval;
  } value;
};

struct lexer
{
  const char *fname;
  FILE *file;
  struct location loc;
  struct token curtoken;
  bool is_eof;
  /* if set true, a token consisting of digits and a-f
     letters will be considering as number.  */
  bool hex_number;
  /* Mark and print possible errors in case of true.
     NOTE When lexer is beyond function, we can skip all errors.  */
  bool error_notifications;
};

struct eq_options
{
  unsigned print_program:1;
  unsigned print_matches:1;
  unsigned print_types:1;
};

extern struct eq_options options;

#   define tval_tok_init(_tok, _cls, _val)             \
    do {                                            \
      (_tok)->tok_class = _cls;                     \
      (_tok)->value.tval = _val;                    \
    } while (0)

#   define cval_tok_init(_tok, _cls, _val)             \
    do {                                            \
      (_tok)->tok_class = _cls;                     \
      (_tok)->value.cval = _val;                    \
    } while (0)

extern const char *token_class_name[];
extern const char *token_kind_name[];
extern const bool is_token_id[];

#   define token_kind_as_string(tkind) token_kind_name[(int) tkind]
#   define token_value(tok)            (tok)->value.tval
#   define token_class(tok)            (tok)->tok_class
#   define token_class_as_string(tcls) token_class_name[(int) tcls]
#   define token_location(tok)         (tok)->loc


__BEGIN_DECLS bool lexer_init (struct lexer *, const char *);
bool lexer_finalize (struct lexer *);
bool is_id (struct token *, bool);
bool token_is_delimiter (struct token *);
struct token *lexer_get_token (struct lexer *);
struct token *token_copy (struct token *);
int token_compare (struct token *, struct token *);
void token_free (struct token *);
void token_print (struct token *);
const char *token_as_string (struct token *);
bool token_uses_buf (struct token *);
__END_DECLS
#endif /* __EXPAND_H__  */
