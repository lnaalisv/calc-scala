package com.lnaalisv.calc

import org.scalatest._

import com.lnaalisv.calc.Utils._
import com.lnaalisv.calc.Tokenizer._

class UtilsSpec extends FlatSpec with Matchers {
    "getMatchingParenthesisIndex" should "get the correct index" in {
        var tokens = List(Plus, Plus, RightParenthesis)
        var index = getMatchingParenthesisIndex(tokens)
        index should be (2)

        tokens = List(
            Plus,
            LeftParenthesis,
            Plus,
            RightParenthesis,
            RightParenthesis,
            Plus
        )

        index = getMatchingParenthesisIndex(tokens)
        index should be (4)

        tokens = List(
            Plus,
            LeftParenthesis,
            Plus,
            LeftParenthesis,
            Plus,
            RightParenthesis,
            RightParenthesis,
            RightParenthesis,
            Plus
        )

        index = getMatchingParenthesisIndex(tokens)
        index should be (7)
    }
}
