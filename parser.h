#ifndef __PARSER_H__
#define __PARSER_H__

#include "expand.h"

struct parser
{
  struct lexer *lex;

  /* Buffer and lengths associated with buffer.
     Buffer holds up-to BUF_SIZE tokens, which means
     that it is possible to look BUF_SIZE tokens
     forward.  */
  struct token **token_buffer;
  size_t buf_size;
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

__BEGIN_DECLS

int parse (struct parser *);
bool parser_init (struct parser *, struct lexer *);
bool parser_finalize (struct parser *);

__END_DECLS

#endif /* __PARSER_H__  */
