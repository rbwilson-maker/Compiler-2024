package edu.cmu.cs411.java.server;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.Socket;
import java.net.SocketException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

/**
 * Edit 1 : Session is no longer a continuous task; instead it performs a single
 * job. Sessions to jobs are 1-to-1.
 */
public class Session extends Thread {
    private final SessionManager manager;
    private final Server server;
    private final Socket reciever;
    final ThreadGroup group;
    private static final Class[] stringArgs = {String[].class};
    
    public Session(SessionManager manager, Socket reciever, Server server, String sessionName) {
        super(new ThreadGroup("group " + sessionName), "thread " + sessionName);
        this.group = getThreadGroup();
        this.manager = manager;
        this.server = server;
        this.reciever = reciever;
    }

    //(CLIENT_NEWJOB[argSiz=K:16][argString:K])*CLIENT_EXIT
    private String readChunk(DataInputStream in) throws IOException {
        final byte[] buffer = new byte[in.readChar()];
        in.readFully(buffer);
        return new String(buffer, "US-ASCII");
    }

    private void readChunks(DataInputStream in, List<String> chunks) throws IOException {
        switch (in.readInt()) {
            case Constants.CLIENT_CONTINUE:
                chunks.add(readChunk(in));
                readChunks(in, chunks); //optimize-tail-recursion.l4
            case Constants.CLIENT_END:
                return;
            default:
                System.err.println("Malformed Client Input");
        }
    }

    private List<String> readChunks(DataInputStream in) throws IOException {
        List<String> chunks = new ArrayList<>();
        readChunks(in, chunks);
        return chunks;
    }

    private List<URL> readResources(DataInputStream in) throws IOException {
        return readChunks(in)
            .parallelStream()
            .map(s -> {
                try {
                    return new File(s).toURI().toURL();
                } catch (MalformedURLException e) {
                    e.printStackTrace();
                    return null;
                }
            })
            .filter(Objects::nonNull)
            .collect(Collectors.toList());
    }

    private void newJob(
        InputStream clientin,
        DataOutputStream clientout,
        int loaderNum,
        List<URL> additionalResources,
        String method,
        List<String> params) throws IOException {

        String[] split = method.split("#");

        assert split.length == 2 : "malformed NEW_JOB request";

        String methodPath = split[1];
        String classPath = split[0];

        try (PrintStream out = new PrintStream(new ServerOutputStream(clientout, Constants.JOB_OUT));
            PrintStream err = new PrintStream(new ServerOutputStream(clientout, Constants.JOB_ERR));) {

            ((ThreadLocalInputStream) System.in).init(clientin);
            ((ThreadLocalPrintStream) System.out).init(out);
            ((ThreadLocalPrintStream) System.err).init(err);

            try {
                try {
                    try {
                        Class<?> mainClass = server.jobClassLoader
                            .loadClass(loaderNum, additionalResources, classPath);

                        mainClass
                            .getMethod(methodPath, stringArgs)
                            .invoke(null, (Object) (params.toArray(new String[0])));

                    } catch (InvocationTargetException e) {
                        throw e.getCause();
                    }
                    throw new ExitException(0);
                } catch (ExitException e) {
                    clientout.writeInt(Constants.JOB_EXIT);
                    clientout.writeInt(e.getCode());
                } catch (ArithmeticException e) {
                    clientout.writeInt(Constants.JOB_EXCEPTION);
                    clientout.writeInt(Constants.EXCEPTION_ARITH);
                } catch (AssertionError e) {
                    clientout.writeInt(Constants.JOB_EXCEPTION);
                    clientout.writeInt(Constants.EXCEPTION_ABORT);
                } catch (TimeoutException e) {
                    clientout.writeInt(Constants.JOB_EXCEPTION);
                    clientout.writeInt(Constants.EXCEPTION_ALARM);
                } catch (ArrayIndexOutOfBoundsException | NegativeArraySizeException | NullPointerException | StackOverflowError e) {
                    e.printStackTrace();
                    clientout.writeInt(Constants.JOB_EXCEPTION);
                    clientout.writeInt(Constants.EXCEPTION_SEGFAULT);
                } catch (ClassNotFoundException | NoSuchMethodException | SecurityException | IllegalAccessException | IllegalArgumentException t) {
                    t.printStackTrace();
                    clientout.writeInt(Constants.JOB_EXCEPTION);
                    clientout.writeInt(Constants.EXCEPTION_INVALID);
                } catch (Throwable c) {
                    c.printStackTrace();
                    clientout.writeInt(Constants.JOB_EXCEPTION);
                    clientout.writeInt(Constants.EXCEPTION_UNKNOWN);
                }
            } catch (SocketException e) {
                //client disconnect
            }
        } finally {
            ((ThreadLocalInputStream) System.in).init(null);
            ((ThreadLocalPrintStream) System.out).init(null);
            ((ThreadLocalPrintStream) System.err).init(null);
        }
    }

    @Override
    public void run() {
        try (DataInputStream sockin = new DataInputStream(reciever.getInputStream());
             DataOutputStream sockout = new DataOutputStream(reciever.getOutputStream())) {

            int type = sockin.readInt();
            switch (type) {
                case Constants.CLIENT_NEWLOADER: {
                    int loaderNum = sockin.readInt();
                    List<URL> resources = readResources(sockin);
                    server.jobClassLoader.newLoaderWithResources(loaderNum, resources);
                    break;
                }
                case Constants.CLIENT_NEWJOB: {
                    int loaderNum = sockin.readInt();
                    List<URL> resources = readResources(sockin);
                    String method = readChunk(sockin);
                    List<String> params = readChunks(sockin);

                    newJob(sockin, sockout, loaderNum, resources, method, params);
                    break;
                }
                case Constants.CLIENT_END: {        //NEW
                    server.shutdown(true);
                    break;
                }
                default:
                    System.err.println("Malformed Client Input");
                    break;
            }

            sockout.flush();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            //ensure we shut down
            shutdown(this);
            
            try{
                manager.shutdownQueue.put(this);
            } catch(InterruptedException e){}
            manager.sessions.remove(this);
        }
    }
    
    static void shutdown(Session s) {
        try {
            final ThreadGroup group = s.group;
            Thread[] threads = new Thread[group.activeCount()];
            group.enumerate(threads, true);
            for (Thread t : threads) {
                if(t == Thread.currentThread()) continue;
                t.stop();
                t.join();
            }
        } catch (InterruptedException e) {}
    }
}
