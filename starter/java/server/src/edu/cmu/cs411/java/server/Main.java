package edu.cmu.cs411.java.server;

public class Main {

    public static final int DEFAULT_TIMEOUT = 1000 * 45;
    public static final int DEFAULT_PORT = 0xBABE;

    /**
     * -s (we are silent, except for errors) -t timeout (millis) -p port
     */
    private static final class Option {

        private long timeout = DEFAULT_TIMEOUT;
        private int port = DEFAULT_PORT;
        private boolean silent = Server.DEFAULT_SILENT;

        private static enum State {
            TIMEOUT,
            PORT,
            NONE
        }

        Option(String[] args) {
            State next = State.NONE;
            try {
                for (String arg : args) {
                    switch (next) {
                        case NONE:
                            switch (arg) {
                                case "-t":
                                    next = State.TIMEOUT;
                                    continue;
                                case "-p":
                                    next = State.PORT;
                                    continue;
                                case "-s":
                                    silent = true;
                                    continue;
                                default:
                                    assert false :
                                        "Invalid flag/argument '" + arg + "'";
                            }
                        case PORT:
                            port = Integer.parseInt(arg);
                            next = State.NONE;
                            break;
                        case TIMEOUT:
                            timeout = Long.parseLong(arg);
                            next = State.NONE;
                            break;
                    }
                }
            } catch (NumberFormatException e) {
                switch (next) {
                    case PORT:
                        assert false : "Error parsing port " + e.getMessage();
                    case TIMEOUT:
                        assert false : "Error parsing timeout " + e.getMessage();
                    default:
                        assert false : "Error while parsing number : " + e.getMessage();
                }
            }

            assert port >= 0 : "Invalid port number " + port;
            assert timeout >= 0 : "Invalid timeout amount " + timeout;
        }
    }

    public static void main(String[] args) {
        Option o = new Option(args);
        Server server = new Server(o.port, o.timeout);
        server.silent = o.silent;

        new Thread(server).start();
    }
}
