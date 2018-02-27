package com.chess

import com.chess.figure.{ChessShape, FigureOnField}

import scala.collection.mutable

/**
  * Created by Administrator on 2/22/2018.
  */
object ThreadBuilder {

  def createWorker(horizontal: Int, vertical: Int, setFigures: Array[ChessShape], chessField: (Int, Int)): Int = {
    if (setFigures.isEmpty) throw new RuntimeException("setFiguresIsEmpty")
    var sortedCollectionFigures = setFigures.sortBy(figure => figure.priority).map(figure => FigureOnField(figure, 1, 1))
    var count: Int = 0
    //    new Thread() {
    //      override def run(): Unit = {
    sortedCollectionFigures(0).horizontal = horizontal
    sortedCollectionFigures(0).vertical = vertical

    abcd(sortedCollectionFigures, chessField, 1)

    def abcd(sortedCollectionFigures: Array[FigureOnField], chessField: (Int, Int), numberFigures: Int): Unit = {
      for (hor <- sortedCollectionFigures(numberFigures).horizontal to chessField._1) {
        for (vert <- sortedCollectionFigures(numberFigures).vertical to chessField._2) {
          sortedCollectionFigures(numberFigures).horizontal = hor
          sortedCollectionFigures(numberFigures).vertical = vert
          if (!isBrokenPlace(sortedCollectionFigures.take(numberFigures), sortedCollectionFigures(numberFigures))) {
            if (numberFigures + 1 == sortedCollectionFigures.length) {
              count += 1
            } else {
              sortedCollectionFigures(numberFigures).horizontal = hor
              sortedCollectionFigures(numberFigures).vertical = vert
              val refactorArray = replacePlaces(sortedCollectionFigures, numberFigures)

              abcd(refactorArray, chessField, numberFigures + 1)
            }
            if (hor == chessField._1 && vert == chessField._2) {
              if (numberFigures == 1) {
                return
              }
              if(sortedCollectionFigures(numberFigures - 1).horizontal == chessField._1
                && sortedCollectionFigures(numberFigures - 1).vertical == chessField._2) {
                abcd(sortedCollectionFigures, chessField, numberFigures - 1)
              }
              if (sortedCollectionFigures(numberFigures - 1).vertical == chessField._2) {
                sortedCollectionFigures(numberFigures - 1).horizontal = sortedCollectionFigures(numberFigures - 1).horizontal + 1
                sortedCollectionFigures(numberFigures - 1).vertical = 1
                abcd(sortedCollectionFigures, chessField, numberFigures - 1)
              } else {
                sortedCollectionFigures(numberFigures - 1).vertical = sortedCollectionFigures(numberFigures - 1).vertical + 1
                abcd(sortedCollectionFigures, chessField, numberFigures - 1)
              }
            }
          }
          if (hor == chessField._1 && vert == chessField._2) {
            if (numberFigures + 1 == sortedCollectionFigures.length) {
              if (numberFigures == 1) return
              if (sortedCollectionFigures(numberFigures - 1).vertical == chessField._2) {
                sortedCollectionFigures(numberFigures - 1).horizontal = sortedCollectionFigures(numberFigures - 1).horizontal + 1
                sortedCollectionFigures(numberFigures - 1).vertical = 1
                abcd(sortedCollectionFigures, chessField, numberFigures - 1)
              } else {
                sortedCollectionFigures(numberFigures - 1).vertical = sortedCollectionFigures(numberFigures - 1).vertical + 1
                abcd(sortedCollectionFigures, chessField, numberFigures - 1)
              }
            }
            //                sortedCollectionFigures(numberFigures).horizontal = hor
            //                sortedCollectionFigures(numberFigures).vertical = vert
            //                abcd(sortedCollectionFigures, chessField, numberFigures + 1)
            return
          }
          sortedCollectionFigures(numberFigures).horizontal = 1
          sortedCollectionFigures(numberFigures).vertical = 1
        }
      }
    }

    //      }
    //    }.start()
    count
  }


  /**
    * checks brokes places
    *
    * @param usedFigures   : Array[ChessShape] - The list of shapes on the chess field
    * @param currentFigure : ChessShape - The current figure
    * @return Boolean- returns true if at least one figure brokes other figure
    */
  private def isBrokenPlace(usedFigures: Array[FigureOnField], currentFigure: FigureOnField): Boolean = {
    for (i <- usedFigures.indices) {
      if (usedFigures(i).figure.brokenField(usedFigures(i).horizontal, usedFigures(i).vertical,
        currentFigure.horizontal, currentFigure.vertical) ||
        currentFigure.figure.brokenField(currentFigure.horizontal, currentFigure.vertical,
          usedFigures(i).horizontal, usedFigures(i).vertical)) {
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
      if(currentPlace == place) return true)
    false
  }
}
