package com.chess

import com.chess.figure._

/**
  * Created by Administrator on 2/21/2018.
  */
object DeterminantFigure {
    def identify(elem: String): ChessShape = {
      elem match {
        case "-q" => Queen()
        case "-k" => King()
        case "-r" => Castle()
        case "-b" => Bishop()
        case "-n" => Knight()
        case so => throw new IllegalArgumentException(s"wrong figure definition ${so}")
      }
    }
}
