#include "eq.h"
#include "options.h"

struct eq_options options;

void
init_options (void)
{
  memset (&options, 0, sizeof options);
}

bool
check_options_consistent (void)
{
  return true;
}



