"""
hack to read all the fish intrinsics names from a cpp source file.
"""

from glob import glob
import re

pattern = re.compile(r'[^"]*"([^"]*)".*')

for f in glob("*library.cpp"):
    print ";", f
    local_funcs = []
    lines = open(f, "r").readlines()
    for line in lines:
        if line.find("registerLibrary") != -1:
            match = pattern.match(line)
            if match:
                print match.groups()[0],
                print " ",
    print
    print
    print
