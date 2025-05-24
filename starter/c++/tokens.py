#!/usr/bin/env python

import sys
import re

inname = sys.argv[1]
outname = sys.argv[2]
state = "waiting"
tokens = {}
maxtoken = 0

with open(inname, "r") as infile:
    with open(outname, "w") as outfile:
        for line in infile:
            # print("{}\t\t{}".format(state,line.strip()))
            if state == "waiting":
                if line.find("enum yytokentype") > 0:
                    state = "onemore"
            elif state == "onemore":
                state = "codes"
            elif state == "codes":
                if line.find("};") > 0:
                    break
                info = line.strip().split(" = ")
                code = re.search('[0-9]+', info[1]).group(0)
                # print("{}\t\t{}\t{}".format(line.strip(), line[0], code))
                tokens[code] = info[0]
                if int(code) > maxtoken:
                    maxtoken = int(code)
        tostr = ["?"] * (maxtoken+1)
        for key in tokens:
            tostr[int(key)] = tokens[key]
        outfile.write("static const char* tokensname[] = {\n")
        for i in range(maxtoken+1):
            outfile.write("\t\"{}\",\t\t// {}\n".format(tostr[i], i))
        outfile.write("\t\"?\"\n};\nconst char* token2name(int x) { if (x > ")
        outfile.write("{}".format(maxtoken))
        outfile.write(") return \"EOB\"; return tokensname[x]; }\n")
