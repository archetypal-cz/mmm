This is the famous and elusive Mystery Mansion that was written by Bill Wolpert back 
in the early 1980s. It was written to run on the HP1000 minicomputer under RTE, and
later ported to the HP3000.

Unlike other versions of Mystery Mansion you may find, this is the real deal - Bill
Wolpert's original FORTRAN code (modified very slightly - see NOTES) and run through
Bellcore's F2C program.

Included is a windows 32 bit executable created by mingw and an MSDOS 16 bit executable
created by Microsoft's QuickC.

BUILDING

If you want to compile your own executables from the included c translated files,
you'll need a copy of Bellcore's f2c program to build the runtime libraries.
IIRC, you need to specify that you want 16 bit integers when building the library.

If you want to use f2c to translate the FORTRAN to c, you'll need to modify the
f2c sources to allow for LOTS of FORTRAN continuation lines. Mystery Mansion 
stores 2 text characters per integer array item, so f2c must be used in 16 bit
translation mode.

NOTES

The HP1000 was a 16 bit mini with a 16 bit logical address space. Prior to RTE6,
a good chunk of a program's address space was taken up by OS tables which forced
programmers to use a segmentation scheme to run programs of any real size. Mystery
Mansion is such a program. It consists of a small main program and several segments
that are loaded as needed. The mechanism for doing this was an EXEC 5 RTE call. 

I've replaced EXEC(5,...) calls in the source with EXEX(5,...). I've also converted
the segments to subroutines. Since segments never do a return
(they just call the next segment), the stack would never get unwound.
To prevent that, my c library contains an exex implementation
in c that uses setjump and longjump to clean up the stack.

Mystery Mansion uses Holerith strings to store data in integer arrays and assumes
a big-endian CPU. To get around the endian problems, in several places in the source,
I equivalence a FORTRAN77 character string to the original integer array holding the
text and use the character string for IO. This eliminates endian dependence.


Ken Cornetet
podad@gmail.com
