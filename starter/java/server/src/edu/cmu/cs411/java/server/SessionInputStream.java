package edu.cmu.cs411.java.server;

import java.io.FilterInputStream;
import java.io.InputStream;

/**
 * 
 * @author zkieda
 */
class SessionInputStream extends FilterInputStream{
    private final InputStream delegate;
    
    SessionInputStream(InputStream delegate){
        super(delegate);
        this.delegate = delegate;
    }
    
    
}
