#!/usr/bin/env python

import sys

inname = sys.argv[1]
outname = sys.argv[2]
state = "waiting"
newin = []

with open(inname, "r") as infile:
    with open(outname, "w") as outfile:
        for line in infile:
            if state == "waiting":
                # copy all input to newinput
                if line.find("/* Token type.  */") >= 0:
                    state = "codes"
                    newin.append("#include \"{}\"\n".format(outname))
                    outfile.write(line)
                else:
                    newin.append(line)
            elif state == "codes":
                # copy all input to outname
                outfile.write(line)
                if line.find("#endif") >= 0:
                    state = "toend"
                    newin.append("\n")
            elif state == "toend":
                newin.append(line)
# now overwrite inname
with open(inname, "w") as outfile:
    for line in newin:
        outfile.write(line)
