package com.evolutiongaming.bootcamp.basics

object BinSearch {

  def search[T](list: List[T], toFind: T)(implicit ev: T => Ordered[T]): Option[Int] = {

    def searchRec[T](list: List[T], toFind: T, start: Int, end: Int)(implicit ev: T => Ordered[T]): Option[Int] = {
      if (start > end) return None

      val middle = Math.round((start + end) / 2)
      if (list(middle) == toFind) return Some(middle)

      if (list(middle) > toFind) {
        searchRec(list, toFind, start, middle - 1)
      } else {
        searchRec(list, toFind, middle + 1, end)
      }
    }

    searchRec(list, toFind, 0, list.length - 1)
  }
}
