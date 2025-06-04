#include <stdio.h>

#include <ev.h>

int
main (void)
{
  printf ("libev version: %d.%d\n", ev_version_major (), ev_version_minor ());
  printf ("fn addr: %p\n", (void *)ev_invoke);
  return 0;
}
