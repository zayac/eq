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

#ifndef __XFILE_H__
#define __XFILE_H__

enum xfile_mode
{
  xf_memory,
  xf_file
};

struct xfile_base
{
  enum xfile_mode  mode;
  size_t bytes_written;
};

struct xfile_file
{
  struct xfile_base base;
  FILE *  f;
  char *  fname;
};

struct xfile_memory
{
  struct xfile_base base;
  char *  buf;
  char *  idx;
  size_t size;
};

union xfile_union
{
  struct xfile_base  base;
  struct xfile_file  f;
  struct xfile_memory m;
};

typedef union xfile_union xfile;


#define XFILE_MODE(xf)  ((xf)->base.mode)
#define XFILE_BYTES_WRITTEN ((xf)->base.bytes_written)

#define XFILE_BUFFER(xf) ((xf)->m.buf)
#define XFILE_INDEX(xf) ((xf)->m.idx)
#define XFILE_SIZE(xf) ((xf)->m.size)

#define XFILE_FILE(xf)  ((xf)->f.f)
#define XFILE_FNAME(xf)  ((xf)->f.fname)


xfile * xfile_init_file (const char *);
xfile * xfile_init_file_stderr ();
xfile * xfile_init_file_stdout ();
xfile * xfile_init_memory (size_t);
void xfile_finalize (xfile *);
int xfile_fprintf (xfile *, const char *, ...);


#endif /* __XFILE_H__  */
