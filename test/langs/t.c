#include <stdlib.h>
#include <stdio.h>
/* #include <string.h> */
// hello world
int main (int argc, const char* argv[]) /* hello world1 */
{
  printf("# of args: %d\n", /* hello world2 */ argc);
  /* hello world3 */

  for (int i = 0; i < argc; i++) {
    printf("arg #%d: %s\n", i, argv[i]);
  }
  /* hello
     world4 */

  return EXIT_SUCCESS;
/* hello world5 */}
