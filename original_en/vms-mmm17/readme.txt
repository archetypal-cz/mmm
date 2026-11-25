Ported from the rev 17 HP fortran sources that are included in
mansion-19.2.tar.gz on the if-archive.
HP specific calls have been emulated in MMMGLUE.FOR
Note that EXEC is implemented as a normal call, while on the HP machines it
was used for loading modules into memory.
Due to the way modules are calling each other, new call frames are pushed onto
the stack at every move. 
This is ugly, but it is not a problem, since the game is ending after 450 moves.
--- Building MMM.EXE
$ @build
--- Testing the 999 points solution for MYSTERY #46:
$ define/user SYS$INPUT solution.txt
$ run mmm
...
YOU HAVE BEATEN THE ODDS AND HAVE DONE THE IMPOSSIBLE. YOU HAVE SURVIVED
MYSTERY MANSION AND YOU CAN NOW SEE IT GOING UP IN SMOKE BEFORE YOU.

YOUR SCORE INCLUDES 92 POINTS FOR THE ITEMS YOU HAVE WITH YOU.

YOUR SCORE ALSO INCLUDES 10 POINTS BECAUSE THE TAXI IS WAITING.

YOU SCORED 999 POINTS WHICH RATES YOU AS A SUPER   
SLEUTH. 
