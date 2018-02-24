#include <stdlib.h>
#include <stdio.h>

int main (int argc, const char* argv[])
{
  printf("# of args: %d\n", argc);

  for (int i = 0; i < argc; i++) {
    printf("arg #%d: %s\n", i, argv[i]);
  }

  return EXIT_SUCCESS;
}
