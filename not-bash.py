#!/usr/bin/env python3
import sys
import sys
import re
import shlex
import subprocess

def p(cmd, echo=True):
    args = shlex.split(cmd)

    try:
        res = subprocess.check_output(args, text=True)
    except subprocess.CalledProcessError as e:
        print(f"\nFAILED:\n\n  {cmd}", file=sys.stderr)
        sys.exit(-1)

    if echo:
        print(res)

    return res

_, *argv = sys.argv

exec(open(argv[0]).read())
