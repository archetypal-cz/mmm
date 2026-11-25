#!/usr/bin/env python3
"""Play MMM in a UTF-8 terminal.

The game works internally in CP852 (byte-level Czech text handling), so this
wrapper converts the player's UTF-8 input to CP852 and the game's CP852
output back to UTF-8.  Save files (ZAPSAT/OBNOVIT) land in the current
directory.
"""
import os
import subprocess
import sys
import threading
from pathlib import Path

EXE = Path(__file__).resolve().parent / "build" / "mmm"


def pump_output(proc):
    out = sys.stdout
    while True:
        chunk = proc.stdout.read1(4096)
        if not chunk:
            break
        out.write(chunk.decode("cp852", "replace"))
        out.flush()


def main():
    env = dict(os.environ, GFORTRAN_UNBUFFERED_ALL="1")
    proc = subprocess.Popen([str(EXE)], stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE, env=env)
    t = threading.Thread(target=pump_output, args=(proc,), daemon=True)
    t.start()
    try:
        for line in sys.stdin:
            if proc.poll() is not None:
                break
            proc.stdin.write(line.encode("cp852", "replace"))
            proc.stdin.flush()
    except (BrokenPipeError, KeyboardInterrupt):
        pass
    finally:
        try:
            proc.stdin.close()
        except BrokenPipeError:
            pass
        proc.wait()
        t.join(timeout=1)
    return proc.returncode


if __name__ == "__main__":
    sys.exit(main())
