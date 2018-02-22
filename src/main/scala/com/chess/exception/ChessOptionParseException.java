package com.chess.exception;

/**
 * Created by Administrator on 2/20/2018.
 */
public class ChessOptionParseException extends RuntimeException {
    public ChessOptionParseException() {
        super();
    }

    public ChessOptionParseException(String message) {
        super(message);
    }

    public ChessOptionParseException(String message, Throwable cause) {
        super(message, cause);
    }
}
