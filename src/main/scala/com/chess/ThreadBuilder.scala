package com.chess

import com.chess.figure.{ChessShape, FigureOnField}

import scala.collection.{SortedSet, mutable}
import scala.util.control.Breaks._

/**
  * Created by Administrator on 2/22/2018.
  */
object ThreadBuilder {

  def createWorker(horizontal: Int, vertical: Int, setFigures: Array[ChessShape], chessField: (Int, Int)): Int = {
    if (setFigures.isEmpty) throw new RuntimeException("setFiguresIsEmpty")
    var sortedCollectionFigures = setFigures.sortBy(figure => figure.priority).map(figure => FigureOnField(figure, 1, 1))
    var freePlaces: mutable.SortedSet[(Int, Int)] = createFreePlaces(chessField)
    var count: Int = 0
    //    new Thread() {
    //      override def run(): Unit = {
    sortedCollectionFigures(0).horizontal = horizontal
    sortedCollectionFigures(0).vertical = vertical

    freePlaces = minusFreePlaces(freePlaces, sortedCollectionFigures(0))

    val freeSquare = freePlaces.headOption
    if(freeSquare.isEmpty) return 0


    worker(sortedCollectionFigures, freePlaces, 1, true)

    def worker(sortedCollectionFigures: Array[FigureOnField], freeSquares: mutable.SortedSet[(Int, Int)], numberFigures: Int, up: Boolean): Unit = {
      val freeSquaresAsArray: Array[(Int, Int)] = freeSquares.toArray
      breakable {
        for (square <- freeSquaresAsArray) {
          if (up) {
            sortedCollectionFigures(numberFigures).horizontal = square._1
            sortedCollectionFigures(numberFigures).vertical = square._2
          }
          if (!isBrokenPlace(sortedCollectionFigures.take(numberFigures), sortedCollectionFigures(numberFigures))) {
            if (numberFigures + 1 == sortedCollectionFigures.length) {
              count += 1
            } else {
              val refactorArray = replacePlaces(sortedCollectionFigures, numberFigures)
              val newFreeSquares = minusFreePlaces(freeSquares, sortedCollectionFigures(numberFigures))
              if (newFreeSquares.isEmpty) {
                break()
              }
              worker(refactorArray, newFreeSquares, numberFigures + 1, true)
            }
            if (square == freeSquaresAsArray.last) {
              if (numberFigures + 1 == sortedCollectionFigures.length) {
                if (numberFigures == 1) return
                val newFreeSquares = plusFreePlaces(chessField, sortedCollectionFigures.take(numberFigures - 1))
                val pair = (sortedCollectionFigures(numberFigures - 1).horizontal, sortedCollectionFigures(numberFigures - 1).vertical)
                if(downToStart(sortedCollectionFigures, newFreeSquares, numberFigures, chessField)) {
                  return
                }
                val newsquare = getNextSquare(newFreeSquares, sortedCollectionFigures(numberFigures - 1))
                sortedCollectionFigures(numberFigures - 1).horizontal = newsquare._1
                sortedCollectionFigures(numberFigures - 1).vertical = newsquare._2
                worker(sortedCollectionFigures, newFreeSquares, numberFigures - 1, false)

              }
              worker(sortedCollectionFigures, freeSquares, numberFigures + 1, true)
            }
          }
        }
      }
    }
    count
  }

  /**
    * checks brokes places
    *
    * @param usedFigures : Array[ChessShape] - The list of shapes on the chess field
    * @param square      : ChessShape - The current figure
    * @return Boolean- returns true if at least one figure brokes other figure
    */
  private def isBrokenPlace(usedFigures: Array[FigureOnField], square: (Int, Int)): Boolean = {
    for (i <- usedFigures.indices) {
      if (usedFigures(i).figure.brokenField(usedFigures(i).horizontal, usedFigures(i).vertical,
        square._1, square._2)) {
        return true
      }
    }
    false
  }

  private def isBrokenPlace(usedFigures: Array[FigureOnField], currentFigure: FigureOnField): Boolean = {
    for (i <- usedFigures.indices) {
      if (currentFigure.figure.brokenField(currentFigure.horizontal, currentFigure.vertical, usedFigures(i).horizontal, usedFigures(i).vertical)) {
        return true
      }
    }
    false
  }

  private def replacePlaces(sortedCollectionFigures: Array[FigureOnField], numberOfFigure: Int): Array[FigureOnField] = {
    val tail = sortedCollectionFigures.takeRight(sortedCollectionFigures.length - numberOfFigure - 1).map(f => f.copy(horizontal = 1, vertical = 1))
    val head = sortedCollectionFigures.take(numberOfFigure + 1)
    head ++ tail
  }

  private def checkSamePlaces(currentPlace: (Int, Int), sortedCollectionFigures: Array[FigureOnField]): Boolean = {
    val usedPlaces = sortedCollectionFigures.map(figureOnField => (figureOnField.horizontal, figureOnField.vertical))
    usedPlaces.foreach(place =>
      if (currentPlace == place) return true)
    false
  }

  private def createFreePlaces(chessField: (Int, Int)): mutable.SortedSet[(Int, Int)] = {
    var freePlaces: mutable.SortedSet[(Int, Int)] = mutable.SortedSet[(Int, Int)]()
    for (hor <- 1 to chessField._1) {
      for (vert <- 1 to chessField._2) {
        val placeOnField = (hor, vert)
        freePlaces += placeOnField
      }
    }
    freePlaces
  }

  private def minusFreePlaces(freePlaces: mutable.SortedSet[(Int, Int)], currentFigure: FigureOnField): mutable.SortedSet[(Int, Int)] = {
    for (place <- freePlaces) {
      if (currentFigure.figure.brokenField(currentFigure.horizontal, currentFigure.vertical, place._1, place._2)) {
        freePlaces -= place
      }
    }
    freePlaces
  }

  private def plusFreePlaces(chessBoard: (Int, Int), figuresOnBoard: Array[FigureOnField]): mutable.SortedSet[(Int, Int)] = {
    var freePlaces: mutable.SortedSet[(Int, Int)] = mutable.SortedSet[(Int, Int)]()
    for (hor <- 1 to chessBoard._1) {
      for (vert <- 1 to chessBoard._2) {
        val placeOnField = (hor, vert)
        if (!isBrokenPlace(figuresOnBoard, placeOnField))
          freePlaces += placeOnField
      }
    }
    freePlaces
  }

  private def getNextSquare(freeSquares: SortedSet[(Int, Int)], figureOnBoard: FigureOnField): (Int, Int) = {
    val pair = (figureOnBoard.horizontal, figureOnBoard.vertical)
    val arr = freeSquares.toList
    val index = arr.indexOf(pair)
    println(arr(index + 1))
    arr(index + 1)
  }

  private def downToStart(sortedCollectionFigures: Array[FigureOnField], freeSquares: SortedSet[(Int, Int)], numberOfFigures: Int, chessBoard: (Int, Int)): Boolean = {
    if (numberOfFigures == 1) return true
    val pair = (sortedCollectionFigures(numberOfFigures).horizontal, sortedCollectionFigures(numberOfFigures).vertical)
    if(freeSquares.last == pair) {
      val newNumberOfFigures = numberOfFigures - 1
      val newFreeSquares = plusFreePlaces(chessBoard, sortedCollectionFigures.take(newNumberOfFigures))
      downToStart(sortedCollectionFigures, newFreeSquares, newNumberOfFigures, chessBoard)
    }else false
  }
}