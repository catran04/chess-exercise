package com.chess

import scala.math.Ordered.orderingToOrdered


/**
  * Created by Administrator on 2/27/2018.
  */
case class PlaceOnField(
                       horizontal: Int,
                       vertical: Int
                       ) {

  implicit def compare(that: PlaceOnField): Ordering[PlaceOnField] = Ordering.by
}
