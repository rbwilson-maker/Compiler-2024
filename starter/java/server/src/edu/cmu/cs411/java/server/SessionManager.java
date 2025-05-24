package edu.cmu.cs411.java.server;

import java.lang.annotation.Annotation;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @author zkieda
 */
public class SessionManager {
    private Server server;
    private volatile boolean shutdown = false;
    
    private AtomicInteger count = new AtomicInteger();
    
    final ArrayBlockingQueue<Session> shutdownQueue 
        = new ArrayBlockingQueue<>(16);
    
    //finish cleaning up threads externally, from a thread external to the 
    //internal thread groups
    private Thread cleanup = new Thread("cleanup") {
        @Override
        public void run() {
            while(!shutdown){
                try{
                    Session s = shutdownQueue.take();
                    
                    ThreadGroup group = s.group;
                    group.suspend();
                    
                    Session.shutdown(s);
                    s.stop();
                    s.join();
                    
                    if(!group.isDestroyed())
                        group.destroy();
                    
                    if(!server.silent) {
                        //print out some additional information to ensure no 
                        //resource leaks
                        ThreadGroup tg = Thread.currentThread()
                            .getThreadGroup();
                        System.out.println("Closing Thread Group");
                        tg.list();
                        System.out.println("Thread Count : " + tg.activeCount() 
                            + ";\tGroup Count : "+tg.activeGroupCount());
                    }
                } catch(InterruptedException e){}
            }
        }
    };
    
        
    SessionManager(Server server) {
        this.server = server;
        cleanup.start();
    }

    //done for fun
    private String newName() {
        return toString() + count.incrementAndGet();
    }

    /**
     * Makes and runs a session from a socket. The session is placed under its
     * own thread
     */
    public void runSession(Socket socket) {
        Session session = new Session(this, socket, server, newName());
        
        synchronized(sessions){
            sessions.add(session);
        }
        
        session.start();
    }

    final List<Session> sessions = new ArrayList<>();

    void shutdown() {
        synchronized(sessions){
            for (Session s : sessions) {
                Session.shutdown(s);
            }
        }
        shutdown = true;
    }
}
