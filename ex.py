"""
Search the source distribution for FISH libraries and extract the function names
"""

from glob import glob
import re
import os
pattern = re.compile(r'[^"]*"([^"]*)".*')

def print_intrinsics(filename):
    local_funcs = []
    lines = open(filename, "r").readlines()
    for line in lines:
        vector_intrinsic = "createVectorInt" in line
        if "registerLibrary" in line or vector_intrinsic:
            match = pattern.match(line)
            if match:
                if vector_intrinsic:
                    print "{0} {0}.x {0}.y {0}.z".format(match.groups()[0]),
                    print " ",
                else:
                    print match.groups()[0],
                    print " ",
    print
    print

if __name__ == '__main__':
    for root, dirs, files in os.walk("."):
        for filename in files:
            if filename.endswith(".cpp") and "library" in filename:
                #print filename
                print_intrinsics(os.path.join(root, filename))
