package com.lnaalisv.calc

import Tokenizer._

object Utils {

    def getMatchingParenthesisIndex(tokens : List[Token], accumulator : Int = 0, level : Int = 0) : Int = {
        val leftIndex = tokens.indexOf(LeftParenthesis)
        val rightIndex = tokens.indexOf(RightParenthesis)
        if(rightIndex == -1) {
            return -1
        } else if (leftIndex != -1 && leftIndex < rightIndex) {
            return getMatchingParenthesisIndex(tokens.takeRight(tokens.length - (leftIndex + 1)), accumulator + leftIndex + 1, level + 1)
        } else if (rightIndex != -1 && level != 0) {
            return getMatchingParenthesisIndex(tokens.takeRight(tokens.length - (rightIndex + 1)), accumulator + rightIndex + 1, level - 1)
        }
        rightIndex + accumulator
    }

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
