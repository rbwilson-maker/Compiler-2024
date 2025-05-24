import os
import sys

## usage: 
# cd dist/bench; python3 bench.py ../tests/bench

C0_PATH = "../reference-compiler/cc0"

## Flags to invoke the reference compiler (safe) with a given opt level.
def cc0flags(file, opt):
    return " ".join(
        [
            "-std=c99",
            "-fwrapv",
            "-I%s/include" % C0_PATH,
            "-I%s/runtime" % C0_PATH,
            "%s.c" % file,
            "-L%s/runtime" % C0_PATH,
            "-Wl,-rpath",
            "%s/runtime" % C0_PATH,
            "../runtime/bench.o",
            "-O%d" % opt,
            "-o %s.exe" % file,
        ]
    )


## Flags to invoke GCC (unsafe) on a syntax-translated C file.
def gccflags(cfile, opt):
    return " ".join(
        [
            "-std=c99",
            "-fwrapv",
            "%s" % cfile,
            "../runtime/bench.o",
            "-O%d" % opt,
            "-o %s.exe" % cfile,
        ]
    )


## Expects all L4 files to be in `folder`, and all --unsafe variants
## to be .C files in an internal folder called `unsafe` within `folder`
def bench(folder):
    os.system("gcc -c -O0 ../runtime/bench.c -o ../runtime/bench.o")
    for filename in sorted(os.listdir(folder)):
        if filename.endswith(".l4"):
            cfile = filename.split(".")[0] + ".c"
            file = folder + "/" + filename
            cfile = folder + "/unsafe/" + cfile
            print('"%s": {' % filename)

            for opt in [0, 1]:
                fixed_flags = cc0flags(file, opt)
                os.system("%s/bin/cc0 -s -ralt %s" % (C0_PATH, file))
                os.system("gcc %s %s/runtime/libalt.so" % (fixed_flags, C0_PATH))
                print('\t"-O%d":         ' % opt, end="", flush=True)
                os.system('echo "$("./%s.exe"),"' % file)
                os.system("rm %s/*.exe %s/*.h %s/*.l4.c" % (folder, folder, folder))

                if os.path.exists(cfile):
                    fixed_flags = gccflags(cfile, opt)
                    os.system("gcc -S %s" % cfile)
                    os.system("gcc %s" % fixed_flags)
                    print('\t"-O%d --unsafe":' % opt, end="", flush=True)
                    os.system('echo "$("./%s.exe"),"' % cfile)

            print("},", flush=True)


if len(sys.argv) != 2 or (not os.path.isdir(sys.argv[1])):
    print("Usage: python3 gcc_bench.py <folder>")
else:
    bench(sys.argv[1])

