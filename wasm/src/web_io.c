/* Browser console for the f2c runtime: stdin lines come from JavaScript.
   Module.mmmReadLine() returns a Promise of a string (null = end of input);
   Module.mmmEncode() turns it into CP852 bytes. */
#include <stdio.h>
#include <emscripten.h>

EM_ASYNC_JS(int, js_readline, (unsigned char *buf, int max), {
  const line = await Module.mmmReadLine();
  if (line === null || line === undefined) return -1;
  const bytes = Module.mmmEncode(line);
  const n = Math.min(bytes.length, max - 1);
  HEAPU8.set(bytes.subarray(0, n), buf);
  HEAPU8[buf + n] = 10;
  return n + 1;
});

static unsigned char lb[512];
static int lpos, llen, at_eof, pushed = -1;

int mmm_getc(FILE *f)
{
    if (f != stdin) return getc(f);
    if (pushed >= 0) { int c = pushed; pushed = -1; return c; }
    if (lpos >= llen) {
        if (at_eof) return EOF;
        fflush(stdout);
        int n = js_readline(lb, sizeof lb);
        if (n < 0) { at_eof = 1; return EOF; }
        llen = n; lpos = 0;
    }
    return lb[lpos++];
}

int mmm_ungetc(int c, FILE *f) { if (f != stdin) return ungetc(c, f); pushed = c; return c; }
int mmm_feof(FILE *f) { return f == stdin ? at_eof : feof(f); }

/* Base address of COMMON /MMBC/ etc. for the UI to read game state. */
extern char mmbc_[], lkeys_[], ipr_[];
EMSCRIPTEN_KEEPALIVE void *mmm_common_mmbc(void) { return mmbc_; }
/* COMMON /MMWEB/ IDEAD (native/prep.py patch_death_flag): 1 while the player
   is dead (reincarnation question, or the game ended by death).  Weak: the
   English engine has no such block and gets NULL. */
extern char mmweb_[] __attribute__((weak));
EMSCRIPTEN_KEEPALIVE void *mmm_common_mmweb(void) { return mmweb_; }
