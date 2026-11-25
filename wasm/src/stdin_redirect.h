/* Force-included into libf2c for the web build: console reads go through
   mmm_getc(), which awaits a line from JavaScript (Asyncify). */
#include <stdio.h>
#undef getc
#undef ungetc
#undef feof
int mmm_getc(FILE *);
int mmm_ungetc(int, FILE *);
int mmm_feof(FILE *);
#define getc(f) mmm_getc(f)
#define ungetc(c, f) mmm_ungetc(c, f)
#define feof(f) mmm_feof(f)
