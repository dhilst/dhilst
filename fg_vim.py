#!/usr/bin/env python3
import sys
import re


def jobs_line(string):
    m = re.match(
        r"^\[(?P<id>\d+)\]\s+(?P<default>\+|-)?\s+(?P<state>.*?)\s+(?P<rest>.*)", string
    )
    if m is not None:
        return m.groupdict()


# I may need something more sophisticated in future
def is_vim(rest):
    return "vim" in rest


if __name__ == "__main__":
    if sys.stdin.isatty():
        sys.stderr.write("Usage: jobs | {}".format(sys.argv[0]))
        sys.exit(-1)
    data = sys.stdin.read().strip().split("\n")
    data = map(jobs_line, data)
    data = filter(lambda x: x is not None, data)
    for d in data:
        if is_vim(d["rest"]):
            print("fg %{id}".format_map(d))
            sys.exit(0)
    else:
        print("/usr/bin/env vim")
