# 15-411 autograder
- Rob Simmons (rjsimmon@cs)
- Thea Brick (tbrick) [s23]
  - seperated everything into files
  - reorganised grading structure

Assumes that the current working directory is the one where the compiler is
built; in particular, tries to read from the following paths:
  `../tests*`
  `../bench*`
  `../runtime`

Assumes that this file is located in: `../harness`

And creates/manipulates the directory: `../log` with a record of what tests have
run.
  - I'm unsure whether this actually happens -- thea (s23)

Running `./runHarness SUITE` runs the harness on whatever testsuite you want
to run.

## Organisation

Prior to Spring23, everything was organised in one file. I (Thea) have attempted
to break it apart into numerous files to make things more maintainable.

Specifically, here is information about the following directories:

- `config` contains configuration/settings for the autograder
- `drive` contains the drivers that run c0c, cc0, etc.
- `grader` computes the information based on the type of grader
- `score` information for computing the score, summarising it, etc.
- `top` high level infrastructure for running the graders
- `utils` all of the utility functions used
- `.molasses` compiled NJ-versions of the files using [molasses](https://github.com/T-Brick/molasses)
  - Importantly, for any changes, you should run `make update-nj` to run
    molasses so that the CM files are up-to-date.


