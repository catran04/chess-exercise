package com.chess

import com.chess.exception.{ChessOptionParseException, EmptyArgsException}
import com.chess.figure.ChessShape
import com.chess.options.EntriesOptions

import scala.collection.mutable

/**
  * Created by Administrator on 2/20/2018.
  */
object appMaster {

  private var shape: ChessShape = _
  private var number: Int = _

  def main(args: Array[String]): Unit = {
    try {
      val parameters = validateParseArgs(args)

    } catch {
      case cope: ChessOptionParseException => {
        println(cope.getMessage)
        System.exit(1)
      }
      case cce: ClassCastException => {
        println(cce.getMessage)
        System.exit(1)
      }
      case nfe: NumberFormatException => {
        println(nfe.getMessage)
        System.exit(1)
      }
    }
  }

  def validateParseArgs(args: Array[String]): EntriesOptions = {
    if(args.isEmpty) throw new EmptyArgsException("args is empty")

    val wight: Int = tryToInt(args(0))
    val height: Int = tryToInt(args(0))



    var chessSet: mutable.Map[ChessShape, Int] = mutable.Map()

    for(i <- 2 until args.length) {
      i % 2 == 0 match {
        case true =>{
          shape = DeterminantFigure.identify(args(i))
        }
        case false => {
          number = args(i).toInt
        }
      }
      val pair = (shape, number)
      chessSet += pair
    }


    EntriesOptions(wight, height, chessSet.toMap)
  }

  def tryToInt(elem: String): Int = {
    try {
      elem.toInt
    } catch {
      case cce: ClassCastException => throw new ClassCastException(s"element of args: '${elem}' should be 'Int'")
      case nfe: NumberFormatException => throw new NumberFormatException(s"element of args: '${elem}' should be 'Int'")
    }

  }
}
