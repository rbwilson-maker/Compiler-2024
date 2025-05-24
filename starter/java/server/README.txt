This is a modified "Nailgun" server. The nailgun server was written by
<a href="http://www.martiansoftware.com/contact.html">Marty Lamb</a>

411 server contact : zkieda@andrew

For useful information : see ./info

The Nailgun server  was modified extensively for the purposes of 411. 
These changes are listed below
    :: Added delegating classloader for jobs
    :: Added job classloader sandboxing with different resources
    :: Added shutdown for jobs that can create sub-threads, and handle their 
       inputs/outputs as well
    :: Changed securitymanager to handle job exits and protect the server 
       runtime.
    :: Changed request protocol 
    :: server autoshutdown 
    :: Removed aliases, alias manager, nail stats, nail versioning (like there 
       are 4 files for describing, finding, and printing the version lol)
    :: Removed client EXIT command. Clients terminate via disconnecting (EOF),
       which is already managed by the underlying socket system
    :: Removed bloat related to NGInputStream / NGOutputStream
    :: Changed package names and class file names.

Files with no code modifications
    :: ThreadLocalInputStream
    :: ThreadLocalPrintStream

Note that every file does not contain an apache license. The full license is 
available in LICENSE.txt.


The original readme is below.

nailgun
=======

Nailgun is a client, protocol, and server for running Java programs from
the command line without incurring the JVM startup overhead.

Programs run in the server (which is implemented in Java), and are 
triggered by the client (written in C), which handles all I/O.

The server and examples are built using maven.  From the project directory,
"mvn clean install" will do it.

The client is built using make.  From the project directory, 
"make && sudo make install" will do it.  To create the windows client
you will additionally need to "make ng.exe".

For more information, see [the nailgun website](http://martiansoftware.com/nailgun/).

