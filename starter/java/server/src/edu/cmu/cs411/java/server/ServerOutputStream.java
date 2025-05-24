package edu.cmu.cs411.java.server;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;

public class ServerOutputStream extends DataOutputStream{
    private final OutputStream out;
    private final int code;
    private final Object lock = new Object();
    
    public ServerOutputStream(OutputStream delegate, final int code) {
        super(delegate);
        this.out = delegate;
        this.code = code;
    }
    
	public void write(int b) throws IOException {
        if((b & (~0xFF)) != 0) super.write(b);
        else write(new byte[]{(byte)(b&0xFF)});
	}
	
	public void write(byte[] b, int offset, int len) throws IOException {
		synchronized(lock) {
            writeInt(code);
            writeChar(len);
			out.write(b, offset, len);
		}
		flush();
	}
}
