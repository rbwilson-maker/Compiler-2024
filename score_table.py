from __future__ import division
import sys, json

# USAGE:
# Run this script on the last line of the output of
# ../timecompiler --autograde. This line is a JSON string
# with the benchmark results in it.
#
# You can do:
#
# ../timecompiler --autograde -m lab5 | tee /dev/tty | tail -n 1 | python3 ../score_table.py
#
# OR
#
# ../timecompiler --autograde | tail -n 1 > res.txt
# python3 ../score_table.py res.txt
#
# Note that the runtime of the benchmarks on your local computer may be different
# from the results Gradescope gets from running your compiler on an AWS instance.
#
# -O0, -O1: Reference compiler cycle counts.
# -O0 --unsafe, -O1 --unsafe: gcc cycle counts.
gcc_runtimes = {
    "albert.l4": {
        "-O0": 12370271091,
        "-O0 --unsafe": 5079160111,
        "-O1": 8561232527,
        "-O1 --unsafe": 1609413791,
    },
    "arrays_and_loops.l4": {
        "-O0": 13885684495,
        "-O0 --unsafe": 7811468360,
        "-O1": 8314140185,
        "-O1 --unsafe": 3451616400,
    },
    "daisy.l4": {
        "-O0": 8226599771,
        "-O0 --unsafe": 2349666103,
        "-O1": 7038992442,
        "-O1 --unsafe": 633293294,
    },
    "danny.l4": {
        "-O0": 3338953891,
        "-O0 --unsafe": 919053131,
        "-O1": 2698864806,
        "-O1 --unsafe": 270996581,
    },
    "fannkuch.l4": {
        "-O0": 56989729429,
        "-O0 --unsafe": 16930486628,
        "-O1": 50816063630,
        "-O1 --unsafe": 7299469874,
    },
    "frank.l4": {
        "-O0": 638345793,
        "-O0 --unsafe": 268314170,
        "-O1": 454127538,
        "-O1 --unsafe": 74814557,
    },
    "georgy.l4": {
        "-O0": 1652174903,
        "-O0 --unsafe": 687969335,
        "-O1": 1262353571,
        "-O1 --unsafe": 333646503,
    },
    "jack.l4": {
        "-O0": 1525136402,
        "-O0 --unsafe": 247723266,
        "-O1": 1256518803,
        "-O1 --unsafe": 85451895,
    },
    "janos.l4": {
        "-O0": 401433814,
        "-O0 --unsafe": 230893125,
        "-O1": 329692874,
        "-O1 --unsafe": 189912461,
    },
    "jen.l4": {
        "-O0": 11877087281,
        "-O0 --unsafe": 11898028670,
        "-O1": 1779444495,
        "-O1 --unsafe": 1776822760,
    },
    "julia.l4": {
        "-O0": 7918135154,
        "-O0 --unsafe": 7016734530,
        "-O1": 4392703550,
        "-O1 --unsafe": 2964764149,
    },
    "leonardo.l4": {
        "-O0": 5049980724,
        "-O0 --unsafe": 4927197701,
        "-O1": 3317159936,
        "-O1 --unsafe": 3300476166,
    },
    "loooops.l4": {
        "-O0": 10643018252,
        "-O0 --unsafe": 8040734025,
        "-O1": 7918717969,
        "-O1 --unsafe": 2530770508,
    },
    "mat.l4": {
        "-O0": 7165323267,
        "-O0 --unsafe": 1871302805,
        "-O1": 6639626160,
        "-O1 --unsafe": 549386605,
    },
    "mist.l4": {
        "-O0": 9438598584,
        "-O0 --unsafe": 9306742690,
        "-O1": 871073449,
        "-O1 --unsafe": 748858365,
    },
    "monica.l4": {
        "-O0": 2369457097,
        "-O0 --unsafe": 2337464001,
        "-O1": 1798129796,
        "-O1 --unsafe": 1782795095,
    },
    "ncik.l4": {
        "-O0": 6828646205,
        "-O0 --unsafe": 3165558735,
        "-O1": 5517595012,
        "-O1 --unsafe": 1149511909,
    },
    "pierre.l4": {
        "-O0": 25839005,
        "-O0 --unsafe": 24518863,
        "-O1": 14587482,
        "-O1 --unsafe": 14284913,
    },
    "ronald.l4": {
        "-O0": 2291566751,
        "-O0 --unsafe": 921633980,
        "-O1": 1782865642,
        "-O1 --unsafe": 396084468,
    },
    "yyb.l4": {
        "-O0": 10623377863,
        "-O0 --unsafe": 2381834857,
        "-O1": 8505183265,
        "-O1 --unsafe": 781631509,
    },
}

gcc_filesizes = {
    "albert.l4": {
        "-O0": 14354,
        "-O0 --unsafe": 9760,
        "-O1": 11770,
        "-O1 --unsafe": 8552,
    },
    "arrays_and_loops.l4": {
        "-O0": 8980,
        "-O0 --unsafe": 7672,
        "-O1": 8244,
        "-O1 --unsafe": 7208,
    },
    "daisy.l4": {
        "-O0": 30605,
        "-O0 --unsafe": 16914,
        "-O1": 19181,
        "-O1 --unsafe": 11858,
    },
    "danny.l4": {
        "-O0": 14072,
        "-O0 --unsafe": 9760,
        "-O1": 10608,
        "-O1 --unsafe": 8352,
    },
    "fannkuch.l4": {
        "-O0": 9088,
        "-O0 --unsafe": 7744,
        "-O1": 8320,
        "-O1 --unsafe": 7240,
    },
    "frank.l4": {
        "-O0": 12979,
        "-O0 --unsafe": 10064,
        "-O1": 11507,
        "-O1 --unsafe": 8608,
    },
    "georgy.l4": {
        "-O0": 18133,
        "-O0 --unsafe": 11952,
        "-O1": 14341,
        "-O1 --unsafe": 10048,
    },
    "jack.l4": {
        "-O0": 14864,
        "-O0 --unsafe": 10160,
        "-O1": 11440,
        "-O1 --unsafe": 8664,
    },
    "janos.l4": {
        "-O0": 13101,
        "-O0 --unsafe": 9624,
        "-O1": 11013,
        "-O1 --unsafe": 8416,
    },
    "jen.l4": {
        "-O0": 30936,
        "-O0 --unsafe": 30392,
        "-O1": 9800,
        "-O1 --unsafe": 9256,
    },
    "julia.l4": {
        "-O0": 11122,
        "-O0 --unsafe": 9088,
        "-O1": 10058,
        "-O1 --unsafe": 8120,
    },
    "leonardo.l4": {
        "-O0": 8600,
        "-O0 --unsafe": 7520,
        "-O1": 8104,
        "-O1 --unsafe": 7040,
    },
    "loooops.l4": {
        "-O0": 8984,
        "-O0 --unsafe": 7904,
        "-O1": 8152,
        "-O1 --unsafe": 7496,
    },
    "mat.l4": {
        "-O0": 9512,
        "-O0 --unsafe": 8336,
        "-O1": 8984,
        "-O1 --unsafe": 7360,
    },
    "mist.l4": {
        "-O0": 8544,
        "-O0 --unsafe": 7848,
        "-O1": 8184,
        "-O1 --unsafe": 7376,
    },
    "monica.l4": {
        "-O0": 8698,
        "-O0 --unsafe": 7616,
        "-O1": 8162,
        "-O1 --unsafe": 6928,
    },
    "ncik.l4": {
        "-O0": 13454,
        "-O0 --unsafe": 9592,
        "-O1": 11422,
        "-O1 --unsafe": 8440,
    },
    "pierre.l4": {
        "-O0": 7600,
        "-O0 --unsafe": 7032,
        "-O1": 7464,
        "-O1 --unsafe": 6904,
    },
    "ronald.l4": {
        "-O0": 13525,
        "-O0 --unsafe": 9232,
        "-O1": 11653,
        "-O1 --unsafe": 8248,
    },
    "yyb.l4": {
        "-O0": 16015,
        "-O0 --unsafe": 12398,
        "-O1": 12407,
        "-O1 --unsafe": 10046,
    },
}

# configuration for how to score
CONSIDER_SIZE = False
DEDUCT_TIMEOUT = False
FAILURE_PENALTY = 0.1  # timeout penalties if DEDUCT_TIMEOUT
DIFF_FACTOR = 0.00
CLAMP = 2.0


class RUNTYPE:
    S0 = "-O0"
    U0 = "-O0 --unsafe"
    S1 = "-O1"
    U1 = "-O1 --unsafe"


class ANSI:
    YELLOW = "\033[33m"
    RED = "\033[31m"
    GREEN = "\033[32m"
    ENDC = "\033[0m"


def compute_multiplier(scores):
    # Apply a FAILURE_PENALTY per failing test if DEDUCT_TIMEOUT
    num_failures = len(gcc_runtimes) - len(scores)
    failure_penalty = float(num_failures) * FAILURE_PENALTY
    if DEDUCT_TIMEOUT:
        result = sum([p for p in scores]) / float(len(scores))
    else:
        result = sum([p for p in scores]) / float(len(gcc_runtimes))
        failure_penalty = 0
    return max(0, result - failure_penalty)


def reference_numbers(v):
    return (
        float(v[RUNTYPE.S0]),
        float(v[RUNTYPE.U0]),
        float(v[RUNTYPE.S1]),
        float(v[RUNTYPE.U1]),
    )


def clamp(score1, score2):
    return min(CLAMP, max(0, score1)), min(CLAMP, max(0, score2))


def compute_scores_helper(results, type):
    assert type == "time" or type == "size"
    idx = 0 if type == "time" else 1
    gcc_numbers = gcc_runtimes if type == "time" else gcc_filesizes

    output = {}
    for k in sorted(gcc_runtimes.keys()):
        if not (k in results.keys()) or results[k] == None:
            continue
        # Refer to the handout for the sources of these names.
        f1 = 1.0 + DIFF_FACTOR
        v = gcc_numbers[k]

        # the times/code sizes of your compiler
        (tc, uc) = results[k][idx]
        (tc, uc) = (float(tc), float(uc))

        # the times/code sizes of the reference compilers (cc0 and gcc)
        (t0, u0, t1, u1) = reference_numbers(v)

        # calculate the score for this benchmark
        ps = f1 - (tc - t1) / (t0 - t1)
        pu = f1 - (uc - u1) / (u0 - u1)
        ps, pu = clamp(ps, pu)

        p = (0.5 * ps) + (0.5 * pu)
        output[k] = p

    return output


def compute_scores(results):
    output = {}
    time_output = compute_scores_helper(results, "time")
    size_output = compute_scores_helper(results, "size")
    for k in sorted(gcc_runtimes.keys()):
        if not (k in results.keys()) or results[k] == None:
            continue

        output[k] = (
            0.8 * time_output[k] + 0.2 * size_output[k]
            if CONSIDER_SIZE
            else time_output[k]
        )

    return output, time_output, size_output


def compute_overall_scores(results):
    return compute_scores(results)[0]


class Alignment:
    L = 0
    R = 1


def to_format_string(header_length, header_alignment, color):
    optional_minus = "-" if header_alignment == Alignment.L else ""
    if color:
        return "%s%%%s%ds%s" % (color, optional_minus, header_length, ANSI.ENDC)
    else:
        return "%%%s%ds" % (optional_minus, header_length)


def print_bench_table(bench, outfile=sys.stdout, doColor=True):
    def printFile(str: str):
        print(str, file=outfile)

    if not bench["compiles"]:
        printFile("Failure to benchmark suite: did not compile.")
        return
    results = bench["results"]
    scores, time_scores, size_scores = compute_scores(results)

    def addColor(c):
        return c if doColor else ""

    def no_color(k):
        return None

    def p_color(k):
        if k < 0.5:
            return addColor(ANSI.RED)
        elif k < 0.8:
            return addColor(ANSI.YELLOW)
        else:
            return addColor(ANSI.GREEN)

    def speedup_color(k):
        if k == None or not (doColor):
            return None
        if k in scores:  # Passed
            return p_color(scores[k])
        else:  # Failed
            return addColor(ANSI.RED)

    # Header, column length, alignment of column.
    headers = [
        ("Benchmark", 20, Alignment.L, no_color),
        ("score", 10, Alignment.R, speedup_color),
        ("GCC -0s", 15, Alignment.R, no_color),
        ("GCC -0u", 15, Alignment.R, no_color),
        ("GCC -1s", 15, Alignment.R, no_color),
        ("GCC -1u", 15, Alignment.R, no_color),
        ("c0c", 15, Alignment.R, no_color),
        ("c0c -u", 15, Alignment.R, no_color),
    ]

    # This function should return a four-tuple corresponding to the
    # values taken at the above columns for a certain file name.
    def selector(k, type):
        assert type == "time" or type == "size"
        idx, gcc_numbers = (0, gcc_runtimes) if type == "time" else (1, gcc_filesizes)
        score_table = time_scores if type == "time" else size_scores
        score = score_table[k] if k in scores else 0
        c0c1 = (
            "{:.2e}".format(results[k][idx][0])
            if k in scores and results[k] != None
            else "--"
        )
        c0cu = (
            "{:.2e}".format(results[k][idx][1])
            if k in scores and results[k] != None
            else "--"
        )
        speedupText = (
            "%.3f" % score if k in scores else ("--" if DEDUCT_TIMEOUT else "%.3f" % 0)
        )
        return (
            k,
            speedupText,
            "{:.2e}".format(gcc_numbers[k][RUNTYPE.S0]),
            "{:.2e}".format(gcc_numbers[k][RUNTYPE.U0]),
            "{:.2e}".format(gcc_numbers[k][RUNTYPE.S1]),
            "{:.2e}".format(gcc_numbers[k][RUNTYPE.U1]),
            c0c1,
            c0cu,
        )

    # The format string for each column.
    def make_format_string(k):
        return "".join(to_format_string(hdr[1], hdr[2], hdr[3](k)) for hdr in headers)

    n = sum(hdr[1] for hdr in headers)

    printFile("\n\nRUN TIMES:")
    printFile(make_format_string(None) % tuple(hdr[0] for hdr in headers))
    printFile("-" * n)

    for k in sorted(gcc_runtimes.keys()):
        printFile(make_format_string(k) % selector(k, "time"))

    m_time = compute_multiplier(time_scores.values())
    color = p_color(m_time)
    printFile(
        "Average speedup (not considering failed): %.3f"
        % (sum(time_scores.values()) / float(len(time_scores.values())))
        if len(time_scores.values()) > 0
        else 0
    )

    if CONSIDER_SIZE:
        printFile("\n\n\nCODE SIZES:")
        printFile(make_format_string(None) % tuple(hdr[0] for hdr in headers))
        printFile("-" * n)

        for k in sorted(gcc_filesizes.keys()):
            printFile(make_format_string(k) % selector(k, "size"))

        m_size = compute_multiplier(size_scores.values())
        color = p_color(m_size)
        printFile(
            "Average code size reduction: %.3f"
            % (sum(size_scores.values()) / float(len(size_scores.values())))
        )

    m = compute_multiplier(scores.values())
    numFailures = len(gcc_runtimes) - len(scores)
    if numFailures > 0:
        printFile(
            "Number of failures: %s%d%s"
            % (addColor(ANSI.RED), numFailures, addColor(ANSI.ENDC))
        )
    printFile("Multiplier: %s%.3f%s" % (addColor(color), m, addColor(ANSI.ENDC)))


if __name__ == "__main__":
    if len(sys.argv) > 2:
        print(
            "Either provide 1 argument (file to read) or 0 arguments (read from stdin)."
        )
        exit(1)

    def process_file(f):
        file = json.load(f)
        print_bench_table(file)

    if len(sys.argv) == 2:
        with open(sys.argv[1]) as f:
            process_file(f)
    else:
        process_file(sys.stdin)
