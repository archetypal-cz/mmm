/* C versions of compat.f90 routines, for the f2c/WebAssembly build. */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include "f2c.h"

/* MS Fortran GETTIM(hour, minute, second, hundredths) */
#ifdef __EMSCRIPTEN__
#include <emscripten.h>
EM_JS(int, mmm_fixed_time, (void), {
  var v = (typeof process !== 'undefined' && process.env && process.env.MMM_FIXED_TIME)
          || (typeof Module !== 'undefined' && Module.MMM_FIXED_TIME);
  return v ? parseInt(v, 10) : -1;
});
#else
static int mmm_fixed_time(void) { const char *v = getenv("MMM_FIXED_TIME"); return v ? atoi(v) : -1; }
#endif

/* MMM_FIXED_TIME=HHMMSS makes the mystery number reproducible (tests). */
int gettim_(shortint *h, shortint *m, shortint *s, shortint *hs)
{
    int ft = mmm_fixed_time();
    if (ft >= 0) { *h = ft / 10000; *m = ft / 100 % 100; *s = ft % 100; *hs = 0; return 0; }
    struct timeval tv; gettimeofday(&tv, NULL);
    struct tm *t = localtime(&tv.tv_sec);
    *h = t->tm_hour; *m = t->tm_min; *s = t->tm_sec; *hs = tv.tv_usec / 10000;
    return 0;
}

/* integer prompt: non-numeric -> 0, EOF -> stop */
int rdint_(shortint *iv)
{
    char line[96];
    int c, n = 0;
    while ((c = getc(stdin)) != EOF && c != '\n')   /* getc: redirectable */
        if (n < (int)sizeof line - 1) line[n++] = (char)c;
    if (c == EOF && n == 0) exit(0);
    line[n] = 0;
    *iv = (shortint)atoi(line);
    return 0;
}

/* f2c-generated code declares s_copy as returning int, libf2c defines it as
   void; WebAssembly traps on such signature mismatches.  The generated C is
   compiled with -Ds_copy=s_copy_i. */
#undef s_copy
extern void s_copy(char *, char *, ftnlen, ftnlen);
int s_copy_i(char *a, char *b, ftnlen la, ftnlen lb) { s_copy(a, b, la, lb); return 0; }
