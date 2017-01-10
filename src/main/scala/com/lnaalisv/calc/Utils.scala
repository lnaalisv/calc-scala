package com.lnaalisv.calc

object Utils {
    implicit class ListUtils[A](list : List[A]) {
        def appendOption(t : Option[A]) : List[A] = {
            t match {
                case Some(value) => list :+ value
                case None => list
            }
        }

        def prependOption(t : Option[A]) : List[A] = {
            t match {
                case Some(value) => value :: list
                case None => list
            }
        }
    }
}
