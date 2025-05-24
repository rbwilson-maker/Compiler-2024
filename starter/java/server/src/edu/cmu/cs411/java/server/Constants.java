package edu.cmu.cs411.java.server;

public class Constants {
    public static final int 
        CLIENT_CONTINUE     = 0xCAFE_00,
        CLIENT_NEWLOADER    = 0xCAFE_01,
        CLIENT_NEWJOB       = 0xCAFE_02,
        CLIENT_END          = 0xCAFE_03,
        
        JOB_EXIT            = 0xCAFE_04,
        JOB_EXCEPTION       = 0xCAFE_05,
        JOB_OUT             = 0xCAFE_06,
        JOB_ERR             = 0xCAFE_07;
    
    public static final int
        EXCEPTION_SEGFAULT  = 0xCAFE_08,
        EXCEPTION_ABORT     = 0xCAFE_09,
        EXCEPTION_ALARM     = 0xCAFE_0A,
        EXCEPTION_ARITH     = 0xCAFE_0B,
        EXCEPTION_UNKNOWN   = 0xCAFE_0C,    //  job throws unknown exception
        EXCEPTION_INVALID   = 0xCAFE_0D;    //  invalid job request - job could never be run
            
}
