package com.chess.exception;

/**
 * Created by Administrator on 2/20/2018.
 */
public class EmptyArgsException extends RuntimeException{
    public EmptyArgsException() {
        super();
    }

    public EmptyArgsException(String message) {
        super(message);
    }

    public EmptyArgsException(String message, Throwable cause) {
        super(message, cause);
    }
}
