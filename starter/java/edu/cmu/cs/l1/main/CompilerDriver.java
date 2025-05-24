// Mike Maxim
// Main compiler driver class

package edu.cmu.cs.l1.main;

import edu.cmu.cs.l1.general.*;
import java.util.*;
import java.io.*;
import edu.cmu.cs.l1.parse.*;
import edu.cmu.cs.l1.translate.*;
import edu.cmu.cs.l1.codegen.*;

public
class CompilerDriver {
 public
  CompilerDriver() {}

  /** Get name without extension */
 private
  static String getRootName(String filename) {
    File file = new File(filename);
    String shortname = file.getName();
    StringTokenizer st = new StringTokenizer(shortname, ".");
    if (file.getParentFile() != null &&
        file.getParentFile().getAbsolutePath() != null)
      return file.getParentFile().getAbsolutePath() + File.separator +
             st.nextToken();
    else
      return st.nextToken();
  }

  /** File exists? */
 private
  static boolean checkFileExistence(String filename) {
    File file = new File(filename);
    return file.exists();
  }

  /** Print usage() information */
 private
  static void usage(GetOpt opts) {
    System.out.println("Usage: c0c [OPTION...] <SOURCEFILE>");
    System.out.println("Where OPTION is:");
    System.out.print(opts.getUsageHelp());
    Globals.shutdown(-1);
  }

  /** Run the preprocessor on the file */
 private
  String preprocess(String file) {
    String cfile = null, ppline;

    // Check to make sure the file is for real
    if (!checkFileExistence(file)) {
      System.out.println("MAIN: Unable to find file: " + file);
      Globals.shutdown(-1);
    }

    // Run the preprocessor
    Runtime rt = Runtime.getRuntime();
    try {
      // Create a temp file for compilation
      cfile = File.createTempFile("__l1_tmp", null).getAbsolutePath();
    } catch (Exception e) {
      System.out.println("MAIN: Unable to create a temporary file, failure");
      Globals.shutdown(-1);
    }

    /*ppline = "cpp -E -C -o " + cfile + " " + file;*/
    ppline = "cp " + file + " " + cfile;
    try {
      rt.exec(ppline).waitFor();
    } catch (Exception e) {
      System.out.println(
          "MAIN: Unable to run pre-processor, check installation");
      System.out.println("MAIN: Shutting Down...");
      Globals.shutdown(-1);
    }

    return cfile;
  }

  /** Process the user options from the command line */
 private
  void processCmdLine(String[] args) {
    GetOpt opts = new GetOpt();
    String mode;
    int mopt, vopt, topt, oopt, uopt, hopt;
    int cygopt;

    mopt = opts.regOption("emit", "e", "output format (abs,x86-64,exe,llvm)",
                          true, false);
    topt = opts.regOption("typecheck-only", "t", "stop after typechecking",
                          false, false);
    vopt = opts.regOption("verbose", "v", "verbose mode", false, false);
    oopt = opts.regOption("opt", "O", "optimization level", true, false);
    uopt = opts.regOption("unsafe", "u", "unsafe runtime", false, false);
    hopt = opts.regOption("lib", "l", "header file to include", true, false);

    try {
      opts.procCommandLine(args, 1);
    } catch (Exception e) {
      usage(opts);
    }

    m_inputfile = opts.getNonOptionArguments();
    m_outputfile = getRootName(m_inputfile) + ".s";

    if (opts.getOption(vopt)) {
      Globals.verboseCompilation = true;
    }

    if (opts.getOption(topt)) {
      Globals.onlyTypecheck = true;
    }

    if (opts.getOption(mopt)) {
      mode = opts.getOptionData(mopt);
      if (mode.equals("abs"))
        m_mode = Compiler.Output.ABS;
      else if (mode.equals("exe"))
        m_mode = Compiler.Output.EXE;
      else if (mode.equals("x86-64"))
        m_mode = Compiler.Output.X86;
      else {
        System.out.println("MAIN: Illegal Mode, see usage help.");
        usage(opts);
      }
    } else {
      m_mode = Compiler.Output.EXE;
    }
  }

 private
  void go(String[] args) throws Exception {
    String ifile;

    processCmdLine(args);
    ifile = preprocess(m_inputfile);

    // Start compilation up to the appropriate stage
    Compiler.go(ifile, m_outputfile, m_mode);
  }

 public
  static void main(String[] args) throws Exception {
    (new CompilerDriver()).go(args);
  }

 private
  String m_inputfile, m_outputfile;
 private
  Compiler.Output m_mode;
}
