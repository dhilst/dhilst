#!/usr/bin/env python3
import sys
import re

if sys.stdin.isatty():
    sys.stderr.write("Usage: jobs | {}".format(sys.argv[0]))
    sys.exit(-1)

data = sys.stdin.read().strip().split("\n")
for d in data:
    if "nvim" in d:
        id = re.match(r"\[(\d+)\]", d).groups(1)[0]
        print(f"fg %{id}")
        sys.exit(0)
else:
    print("nvim")
