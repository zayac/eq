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

#ifndef __EQ_H__
#define __EQ_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>

#ifndef SAC_SOURCE
#  ifndef __cplusplus
      typedef unsigned char bool;
      #define true 1
      #define false 0
#  endif
#else
   /* bool type is defined by SAC.
      XXX what if sizeof (bool) won't match?  */
   #define false FALSE
   #define true TRUE
#endif

#ifdef __cplusplus
#include <cstdio>
#endif

#define LEXER_BUFFER  8192

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


extern int eq_error_count;
extern int eq_warning_count;

#define assert(expr, ...) \
  ((expr) ? (void)0 \
                : (void)(fprintf (stderr, "%s:%i %s: Assertion (" \
                                     # expr ") failed.\n", \
                                     __FILE__, __LINE__, __func__), \
                    xfprintf (stderr, __VA_ARGS__), \
                    abort ()))

#define unreachable(...)  \
  ((void) (fprintf (stderr, "Code in %s:%d reached impossible state.\n", \
                    __FILE__, __LINE__), \
           xfprintf (stderr, __VA_ARGS__), \
           abort ()))

#define error_loc(loc, ...) \
  do {  \
    (void) fprintf (stderr, "error:%d:%d: ", (int)loc.line, (int)loc.col); \
    (void) fprintf (stderr, "[line=%i]  ", __LINE__); \
    (void) xfprintf (stderr, __VA_ARGS__); \
    ++eq_error_count; \
  } while (0)

#define error( ...) \
  do {  \
    (void) fprintf (stderr, "error: "); \
    (void) xfprintf (stderr, __VA_ARGS__); \
    ++eq_error_count; \
  } while (0)

#define warning_loc(loc, ...) \
  do {  \
    (void) fprintf (stderr, "warning:%d:%d: ", (int)loc.line, (int)loc.col); \
    (void) xfprintf (stderr, __VA_ARGS__); \
    ++ eq_warning_count; \
  } while (0)

#define warning(...) \
  do {  \
    (void) fprintf (stderr, "warning: "); \
    (void) xfprintf (stderr, __VA_ARGS__); \
    ++ eq_warning_count; \
  } while (0)

#define TOKEN_KIND(a, b) a,
#define KEYWORD(a, b, c, d) tv_ ## a,
enum eq_token_kind
{
#include "token_kind.def"
#include "keywords.def"
  eq_tok_kind_length
};
#undef TOKEN_KIND
#undef KEYWORD

#define TOKEN_CLASS(a, b) tok_ ## a,
enum eq_token_class
{
#include "token_class.def"
  eq_tok_class_length
};
#undef TOKEN_CLASS

struct eq_location
{
  size_t line, col;
};

struct eq_token
{
  struct eq_location loc;
  enum eq_token_class tok_class;
  bool uses_buf;
  union
  {
    char *cval;
    enum eq_token_kind tval;
  } value;
};

struct eq_lexer
{
  const char *fname;
  FILE *file;
  struct eq_location loc;
  struct eq_token curtoken;
  bool is_eof;
  /* if set true, a token consisting of digits and a-f
     letters will be considering as number.  */
  bool hex_number;
  /* Mark and print possible errors in case of true.
     NOTE When lexer is beyond function, we can skip all errors.  */
  bool error_notifications;
};

enum break_options
{
  break_nothing,
  break_parser,
  break_typecheck,
  break_controlflow,
  break_dataflow
};

struct eq_options
{
  unsigned print_program:1;
  unsigned print_matches:1;
  unsigned print_types:1;
  enum break_options break_option;
};


#define tval_tok_init(_tok, _cls, _val)             \
    do {                                            \
      (_tok)->tok_class = _cls;                     \
      (_tok)->value.tval = _val;                    \
    } while (0)

#define cval_tok_init(_tok, _cls, _val)             \
    do {                                            \
      (_tok)->tok_class = _cls;                     \
      (_tok)->value.cval = _val;                    \
    } while (0)

extern const char *eq_token_class_name[];
extern const char *eq_token_kind_name[];
extern const bool eq_is_token_id[];

#define eq_token_kind_as_string(tkind) eq_token_kind_name[(int) tkind]
#define eq_token_value(tok)            (tok)->value.tval
#define eq_token_class(tok)            (tok)->tok_class
#define eq_token_class_as_string(tcls) eq_token_class_name[(int) tcls]
#define eq_token_location(tok)         (tok)->loc


__BEGIN_DECLS
bool eq_lexer_init (struct eq_lexer *, const char *);
bool eq_lexer_init_file (struct eq_lexer *, FILE *, const char *);
bool eq_lexer_finalize (struct eq_lexer *);
bool eq_is_id (struct eq_token *, bool);
bool eq_token_is_delimiter (struct eq_token *);
struct eq_token *eq_lexer_get_token (struct eq_lexer *);
struct eq_token *eq_token_copy (struct eq_token *);
int eq_token_compare (struct eq_token *, struct eq_token *);
void eq_token_free (struct eq_token *);
void eq_token_print (struct eq_token *);
const char *eq_token_as_string (struct eq_token *);
bool eq_token_uses_buf (struct eq_token *);
__END_DECLS
#endif /* __EQ_H__  */
