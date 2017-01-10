package com.lnaalisv.calc

import org.scalatest._

import ASTStep._
import Tokenizer._

class ASTStepSpec extends FlatSpec with Matchers {
    "convertToFinalIfNeeded" should "do nothing if given a Final" in {
        val converted = convertToFinalIfNeeded(Final(Value(1)))
        converted should be (Final(Value(1)))
    }

    it should "do nothing if there are more tokens left" in {
        val step = Step(Some(Value(1)), List(Plus))
        val converted = convertToFinalIfNeeded(step)
        converted should be (step)
    }

    it should "convert to Final if there are no more tokens" in {
        val step = Step(Some(Value(1)), List())
        val converted = convertToFinalIfNeeded(step)
        converted should be (Final(Value(1)))
    }
}
