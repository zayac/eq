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
#include <stdarg.h>
#include <err.h>

#include "eq.h"
#include "xfile.h"


xfile *
xfile_init_file_stdout ()
{
  xfile *  xf;

  xf = (xfile *) malloc (sizeof (struct xfile_file));
  if (xf == NULL)
    {
      warn ("malloc");
      goto fail;
    }

  XFILE_FILE (xf) = stdout;
  XFILE_FNAME (xf) = strdup ("<stdout>");
  XFILE_MODE (xf) = xf_file;
  return xf;

fail:
  if (xf)
    free (xf);

  return NULL;
}


xfile *
xfile_init_file_stderr ()
{
  xfile *  xf;

  xf = (xfile *) malloc (sizeof (struct xfile_file));
  if (xf == NULL)
    {
      warn ("malloc");
      goto fail;
    }

  XFILE_FILE (xf) = stderr;
  XFILE_FNAME (xf) = strdup ("<stderr>");
  XFILE_MODE (xf) = xf_file;
  return xf;

fail:
  if (xf)
    free (xf);

  return NULL;
}


xfile *
xfile_init_file (const char *  fname)
{
  xfile *  xf;

  assert (fname != NULL, "xfile initialized with empty filename");

  xf = (xfile *) malloc (sizeof (struct xfile_file));
  if (xf == NULL)
    {
      warn ("malloc");
      goto fail;
    }

  XFILE_FILE (xf) = fopen (fname, "w");
  if (! XFILE_FILE (xf))
    {
      warn ("fopen");
      goto fail;
    }

  XFILE_FNAME (xf) = strdup (fname);
  XFILE_MODE (xf) = xf_file;
  return xf;

fail:
  if (xf)
    {
      if (XFILE_FILE (xf) && XFILE_FILE (xf) != stdout
	  && XFILE_FILE (xf) != stderr)
	fclose (XFILE_FILE (xf));

      free (xf);
    }

  return NULL;
}

xfile *
xfile_init_memory (size_t size)
{
  xfile *  xf;

  if (size == 0)
    size = 32;

  xf = (xfile *) malloc (sizeof (struct xfile_memory));
  if (xf == NULL)
    {
      warn ("malloc");
      goto fail;
    }

  XFILE_BUFFER (xf) = (char *) malloc (sizeof (char) * size);
  if (XFILE_BUFFER (xf) == NULL)
    {
      warn ("malloc");
      goto fail;
    }

  XFILE_INDEX (xf) = XFILE_BUFFER (xf);
  XFILE_SIZE (xf) = size;
  XFILE_MODE (xf) = xf_memory;
  return xf;

fail:
  if (xf)
    {
      if (XFILE_BUFFER (xf))
	free (XFILE_BUFFER (xf));

      free (xf);
    }

  return NULL;
}


void
xfile_finalize (xfile *  xf)
{
  assert (xf != NULL, "attempt to free empty xfile.");

  if (XFILE_MODE (xf) == xf_file)
    {
      if (XFILE_FILE (xf))
	{
	  fflush (XFILE_FILE (xf));
	  fclose (XFILE_FILE (xf));
	}
      free (XFILE_FNAME (xf));
      free (xf);
    }

  else if (XFILE_MODE (xf) == xf_memory)
    {
      if (XFILE_BUFFER (xf))
	free (XFILE_BUFFER (xf));
      free (xf);
    }
  else
    unreachable ("invalid xfile mode found");
}


int
xfile_fprintf (xfile *  xf, const char * fmt, ...)
{
  int ret;
  va_list ap;

  if (XFILE_MODE (xf) == xf_file)
    {
      va_start (ap, fmt);
      ret = vfprintf (XFILE_FILE (xf), fmt, ap);
      va_end (ap);

      return ret;
    }

  else if (XFILE_MODE (xf) == xf_memory)
    {
      size_t n, pos;

      assert (XFILE_INDEX (xf) <= XFILE_BUFFER (xf) + XFILE_SIZE (xf),
	      "xfile index is outside the allocated buffer");

      while (1)
	{
	  pos = XFILE_INDEX (xf) - XFILE_BUFFER (xf);
	  n = XFILE_SIZE (xf) - pos;

	  /* Try to print the message in the existing buffer.  */
	  va_start (ap, fmt);
	  ret = vsnprintf (XFILE_INDEX (xf), n, fmt, ap);
	  va_end (ap);

	  /* In case the process succeeded.  */
	  if (ret > -1 && (size_t)ret < n)
	    {
	      XFILE_INDEX (xf) += ret;
	      return ret;
	    }

	  XFILE_SIZE (xf) *= 2;
	  XFILE_BUFFER (xf) = (char *) realloc (XFILE_BUFFER (xf),
						sizeof (char)
						* XFILE_SIZE (xf));
	  if (XFILE_BUFFER (xf) == NULL)
	    {
	      warn ("realloc");
	      return -1;
	    }

	  XFILE_INDEX (xf) = XFILE_BUFFER (xf) + pos;
	}

    }
  else
    unreachable ("undefined xfile mode");
}

#if 0
int
main (int argc, char *argv[])
{
  xfile * xf;

  xf = xfile_init_file_stdout ();

  xfile_fprintf (xf, "%2.3f - %s ", 3.14567, "kaka");
  xfile_fprintf (xf, "%2.3f - %s ", 3.14567, "kaka");
  xfile_fprintf (xf, "%2.3f - %s ", 3.14567, "kaka");
  xfile_fprintf (xf, "%2.3f - %s ", 3.14567, "kaka");

  printf ("%s", XFILE_BUFFER (xf));
  xfile_finalize (xf);

  return 0;
}
#endif

