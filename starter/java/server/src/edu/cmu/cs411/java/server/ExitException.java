package edu.cmu.cs411.java.server;

class ExitException extends RuntimeException{
    private final int code;
    public ExitException(int exitCode){
        this.code = exitCode;
    }
    public int getCode() {
        return code;
    }
}
