package edu.cmu.cs411.java.server;

import java.io.InputStream;
import java.io.PrintStream;
import java.lang.reflect.Method;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

public class Server implements Runnable {
    public static final boolean DEFAULT_SILENT = false;
    private final int port;
    private final long timeout;
    private volatile boolean running = false;
    private boolean shutdown = false;

    private final SessionManager sessionManager;
    private final PrintStream out = System.out;
    private final PrintStream err = System.err;
    private final InputStream in = System.in;
    private SecurityManager originalSecurityManager = null;

    private ServerSocket serversocket;
    
    private TimerTask timeoutTask;
    private final Timer timeoutTimer;
    
    boolean silent = DEFAULT_SILENT;
    
    final JobClassLoader jobClassLoader = 
        new JobClassLoader();
    
    private final class TimeoutTask extends TimerTask {

        private volatile boolean isCanceled = false;

        @Override
        public void run() {
            //used in C0++ for long-running applications hosted
            //on the server.
            
            //should not happen in c0 tests
            
//            synchronized(sessionManager.sessions){
//                if(!sessionManager.sessions.isEmpty()){
//                    synchronized(Server.this){
//                        pushbackTimeout();
//                        return;
//                    }
//                }
//            }

            synchronized (Server.this) {
                if (isCanceled) {
                    return;
                }
            }
            
            if(!silent) System.out.println("Server Timeout.");
            shutdown(true);
        }

        @Override
        public boolean cancel() {
            synchronized (this) {
                isCanceled = true;
            }
            return super.cancel();
        }
    }

    public Server(int port, long timeout) {
        this.port = port;
        this.timeout = timeout;

        timeoutTimer = new Timer("timeout task");
        timeoutTask = new TimeoutTask();
        timeoutTimer.schedule(timeoutTask, timeout);
        sessionManager = new SessionManager(this);
    }

    private void pushbackTimeout() {
        timeoutTask.cancel();
        timeoutTask = new TimeoutTask();
        timeoutTimer.schedule(timeoutTask, timeout);
    }

    @Override
    public void run() {
        running = true;

        originalSecurityManager = System.getSecurityManager();
        System.setSecurityManager(
            new SessionSecurityManager(originalSecurityManager));

        synchronized (System.in) {
            if (!(System.in instanceof ThreadLocalInputStream)) {
                System.setIn(new ThreadLocalInputStream(in));
                System.setOut(new ThreadLocalPrintStream(out));
                System.setErr(new ThreadLocalPrintStream(err));
            }
        }

        try {
            try{
                serversocket = new ServerSocket(port);
            } catch(BindException e) {
                if(!silent) {
                    System.out.println("Port 0x" + Integer.toHexString(port).toUpperCase() + " is in use.");
                    System.out.println("Exiting under the assumption that another server is already running.");
                }
                shutdown(true);
            }
            
            while (!shutdown) {
                Socket socket = serversocket.accept();
                pushbackTimeout();
                sessionManager.runSession(socket);
            }

        } catch (Throwable t) {
            if (!shutdown) {
                t.printStackTrace();
            }
        }
        running = false;
    }

    public void shutdown(boolean exitVM) {
        synchronized (this) {
            if (shutdown) {
                return;
            }
            shutdown = true;
        }

        try {
            serversocket.close();
            sessionManager.shutdown();
        } catch (Throwable toDiscard) {}


        // restore system streams
        System.setIn(in);
        System.setOut(out);
        System.setErr(err);

        System.setSecurityManager(originalSecurityManager);

        if (exitVM) {
            System.exit(0);
        }
    }

    public void shutdownHook() {
        int count = 0;
        while (running && (count < 50)) {

            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {}
            count++;
        }
    }
}
