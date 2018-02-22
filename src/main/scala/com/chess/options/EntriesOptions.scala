package com.chess.options

import com.chess.figure.ChessShape

/**
  * Created by Administrator on 2/20/2018.
  */
case class EntriesOptions(
                           width: Int,
                           height: Int,
                           shapes: Map[ChessShape, Int]
                         ) {

}
