A compiler does exactly what you think it does, but very carefully. In this course, you will likewise take a lot of care to do exactly what you want.

This is the public repository for 15-411, Compiler Design, at Carnegie Mellon University. Test cases, runtime files, and useful scripts will be released here throughout the semester.

### Overview

This directory contains the following directories:

 * `harness`: The grading harness, implemented in SML.
 * `runtime`: Runtime files that the generated x86 will be linked against.
 * `starter`: Starter code for the lab1 compiler in different languages.
 * `tests`: The tests we provide to you.

### Getting started

After cloning this repository, you should clone your team's repository to a subdirectory thereof.

    $ git clone https://github.com/15-411-S24/dist
    $ cd dist
    $ git clone https://github.com/15-411-S24/<GROUPNAME> compiler

(Replace `<GROUPNAME>` with the name of your group.)

Then, you can prepopulate your repository with starter code.

    $ cp -R starter/ocaml compiler/lab1
    $ cd compiler
    $ git add . # Commit and push, too.
    $ make lab1

### Running code

The starter code is a mostly-working compiler, but instead of compiling to X86-64 assembly (a \*.l1.s file), it compiles to an abstract 3-address assembly (a \*.l1.abs file)

    # From the compiler directory, after running make.
    $ bin/c0c tests/first.l1

This creates the file tests/first.l1.abs, which you can examine.

### Testing code

You can test code using the grading harness.
The `gradecompiler` script should always be invoked from within your `compiler` directory, since it expects to find your compiler at the relative path `bin/c0c` after invoking `make`.
From the `compiler` directory, run:

    $ ../gradecompiler ../tests/l1-basic -m lab1

Use the `-h` flag to learn about more options of the compiler grader.

### Setting up and using Docker

You will likely want to do your development locally, but your local environment differs in many aspects from the environment your code is graded on. For this reason, we make a Docker image available that exactly replicates the environment your code will be graded under. You can use this Docker image for running the test suite, for example.

The steps for installing Docker differ from OS to OS:
  * On Linux, you can follow these steps: <https://docs.docker.com/install/linux/docker-ce/ubuntu/#install-using-the-repository>. You should run the command `sudo usermod -aG docker $USER` after installing Docker so that your user doesn't need sudo to run Docker; otherwise, you'll have to prefix all docker commands with `sudo`.
  * On Windows, you can install [Docker Desktop for Windows](https://docs.docker.com/docker-for-windows/install/). After downloading, installing, and starting Docker per the linked instructions, you should open up PowerShell as an administrator and run `docker --version` to ensure that Docker correctly installed.
  * On OSX, you can can install [Docker Desktop for Mac](https://docs.docker.com/docker-for-mac/install/).

The course staff makes the following images available: `cmu411/autograder-ocaml`, `cmu411/autograder-sml`, `cmu411/autograder-haskell`, `cmu411/autograder-rust`, and `cmu411/autograder-other`. These images have the respective language environments pre-installed. The `autograder-other` image can have your favorite language environment installed on it, so don't be afraid to ask.

Here are the main docker commands you will need. In all of the below commands, replace `IMAGE` with the appropriate image name (perhaps `cmu411/autograder-ocaml`).
  * `docker pull IMAGE:latest`: This command pulls the latest version of the image from DockerHub. You'll want to run this when the course staff updates the image with, say, more OCaml packages.
  * `docker run --name 411 -td IMAGE:latest`: This command creates a new _container_ (a running instance of an image) named 411 and starts running it in the background.
  * `docker cp FILE 411:/autograder`: This command copies `FILE` from your local machine to the `/autograder` directory on the instance. You can likewise copy from a running instance back to your local machine.
  * `docker exec -it 411 bash`: This command executes the command `bash` inside the container `411`; that is, it places you inside a shell inside the running container. From inside the container, you can clone the `dist` repo and the `compiler` repo, and then run tests. You can use the command `exit` (or `Ctrl+D`) to exit the container shell and return to your host shell.
  * `docker stop 411`: This command stops the `411` container from running. Don't do this until you are done with the instance, but you can still use `docker cp` to copy files from a stopped container.
  * `docker rm 411`: This command removes the `411` container. Don't do this until you are done with the instance; the files cannot be recovered.

**Important note:** If you restart your machine or your system crashes, any currently-running Docker containers will become stopped. To recover files you had on a stopped docker container, you can still use `docker cp`, but you CANNOT copy files from a container after you remove it.

If you regularly find yourself wanting additional software inside the Docker container, you can create your own image based on the 15-411 image with the additional software installed.

```
# In a file called "Dockerfile"
FROM cmu411/autograder-haskell:latest

# This is just an example of installing the mercurial package.
RUN apt-get install -y mercurial
```

Then, you can use the command `docker build -t my-411-image:latest .` to build an image called `my-411-image` (instead of `cmu411/autograder-haskell`). Then, you can use Docker to create a container based on `my-411-image` instead of `cmu411/autograder-haskell` by running: `docker run --name 411 -td my-411-image:latest`.

The course staff makes the current version of the original Dockerfile available [in this GitHub repository](https://github.com/15-411/worker-docker).
