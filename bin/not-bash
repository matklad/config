#!/usr/bin/env python3
import sys
import re
import shlex
import subprocess

def p(cmd, echo=True):
    args = shlex.split(cmd)

    try:
        res = subprocess.check_output(args, text=True)
    except subprocess.CalledProcessError as e:
        eprint(f"\nFAILED:\n\n  {cmd}")
        exit(-1)

    if echo:
        print(res)

    return res

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

_, *argv = sys.argv
exit = sys.exit

exec(open(argv[0]).read())
