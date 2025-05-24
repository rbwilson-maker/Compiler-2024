package edu.cmu.cs411.java.server;

import java.security.Permission;

/**
 * Prevents jobs from exiting. Installed for each resource loader.
 * We also prevent jobs from doing funky stuff that can induce race conditions
 * or cause the job to run forever
 */
public class SessionSecurityManager extends SecurityManager {
    private final SecurityManager base;

    public SessionSecurityManager(SecurityManager base) {
        this.base = base;
    }

    public void checkPermission(Permission perm) {
        if (base != null) {
            base.checkPermission(perm);
        }
    }
    
    //NOTE : It is possible for closing issues to come up in two cases
    //  1) when the job thread grabs our parent ThreadGroup, and makes threads external to their sandbox
    //  2) when the job thread interrupts the parent thread as its shutting down
    //It is possible to prevent this using a security manager
    
//    public void checkAccess(ThreadGroup g) {
//        if (baseThreadGroup.parentOf(g)) {
//            throw new SecurityException();
//        }
//        
//        if (base != null) {
//            base.checkAccess(g);
//        }
//    }
//
//    @Override
//    public void checkAccess(Thread t) {
//        if(baseThreadGroup.parentOf(t.getThreadGroup())){
//            throw new SecurityException();
//        }
//            
//        if(base != null) {
//            base.checkAccess(t);
//        }
//    }
    
    

    public void checkPermission(Permission perm, Object context) {
        if (base != null) {
            base.checkPermission(perm, context);
        }
    }

    public void checkRead(String file) {
        if (base != null) {
            base.checkRead(file);
        }
    }

    public void checkExit(int status) {
        throw new ExitException(status);
    }
}