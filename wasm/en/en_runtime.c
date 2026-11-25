/* HP 1000 RTE system routines for Bill Wolpert's Mystery Mansion.
   Based on Ken Cornetet's STUBS.C (expressly released to the public domain),
   adapted for WebAssembly: segments are plain subroutine calls instead of
   setjmp/longjmp (which cannot be combined with asynchronous input), like
   the VMS port.  The stack grows a little with every segment change, which
   is harmless: f2c keeps locals static and the game ends after 450 moves. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include "f2c.h"

int mmm_fixed_time(void);   /* native/compat.c: MMM_FIXED_TIME for reproducible tests */

/* EXEC 11: time of day -> itm[1..4] = sec, min, hour, day of year */
int exec_(shortint *code, shortint *itm)
{
    int ft = mmm_fixed_time();
    (void)code;
    if (ft >= 0) {
        itm[0] = 0; itm[1] = ft % 100; itm[2] = ft / 100 % 100; itm[3] = ft / 10000; itm[4] = 1;
        return 0;
    }
    time_t t = time(NULL);
    struct tm *tm = localtime(&t);
    itm[0] = 0; itm[1] = tm->tm_sec; itm[2] = tm->tm_min; itm[3] = tm->tm_hour; itm[4] = tm->tm_yday + 1;
    return 0;
}

/* segment "loading": EXEX -> EXEXC(name, p1..p5) */
static shortint ipx[5];
extern int mmsa_(void), mmsb_(void), mmsc_(void), mmsd_(void), mmse_(void), mmsf_(void),
           mmsg_(void), mmsh_(void), mmsi_(void), mmsj_(void), mmsk_(void), mmsl_(void);

int exexc_(char *name, shortint *i1, shortint *i2, shortint *i3, shortint *i4, shortint *i5, ftnlen namelen)
{
    ipx[0] = *i1; ipx[1] = *i2; ipx[2] = *i3; ipx[3] = *i4; ipx[4] = *i5;
    char seg = namelen > 3 ? name[3] : '?';
    switch (seg) {
    case 'A': mmsa_(); break;  case 'B': mmsb_(); break;  case 'C': mmsc_(); break;
    case 'D': mmsd_(); break;  case 'E': mmse_(); break;  case 'F': mmsf_(); break;
    case 'G': mmsg_(); break;  case 'H': mmsh_(); break;  case 'I': mmsi_(); break;
    case 'J': mmsj_(); break;  case 'K': mmsk_(); break;  case 'L': mmsl_(); break;
    default: fprintf(stderr, "unknown segment %.6s\n", name); exit(1);
    }
    /* a segment returned instead of chaining: the game is over */
    exit(0);
    return 0;
}

int rmpax_(shortint *ip) { memcpy(ip, ipx, sizeof ipx); return 0; }

/* FMGR file calls used by SUSPEND/RESTORE */
int open_(shortint *idcb, shortint *ierr, char *namf, shortint *creat)
{
    char name[130];
    int i;
    for (i = 0; i < 128 && namf[i] && namf[i] != ' '; i++) name[i] = namf[i];
    name[i] = 0;
    int fd = open(name, O_RDWR | (*creat ? O_CREAT | O_TRUNC : 0), 0666);
    *idcb = (shortint)fd;
    *ierr = fd < 0 ? -1 : 0;
    return 0;
}
int readf_(shortint *idcb, shortint *ierr, shortint *buf, shortint *len)
{ *ierr = read(*idcb, buf, *len * 2) < 0 ? -1 : 0; return 0; }
int writf_(shortint *idcb, shortint *ierr, shortint *buf, shortint *len)
{ *ierr = write(*idcb, buf, *len * 2) < 0 ? -1 : 0; return 0; }
int close_(shortint *idcb) { close(*idcb); return 0; }
