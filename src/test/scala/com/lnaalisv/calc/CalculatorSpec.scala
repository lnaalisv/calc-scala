package com.lnaalisv.calc

import org.scalatest._
import Calculator._

class CalculatorSpec extends FlatSpec with Matchers {

    val Plus = Operator('+')
    val Minus = Operator('-')
    var Times = Operator('*')
    var Divide = Operator('/')

    "calculateExpression" should "calculate simple calculations" in {
        var calculation = Calculation(1, Plus, 1)
        calculateExpression(calculation) should be (2)

        calculation = Calculation(1, Minus, 1)
        calculateExpression(calculation) should be (0)

        calculation = Calculation(2, Times, 3)
        calculateExpression(calculation) should be (6)

        calculation = Calculation(1, Divide, 2)
        calculateExpression(calculation) should be (0.5)
    }

    it should "calculate nested calculations" in {
        var calculation = Calculation(
            1,
            Plus,
            Calculation(
                2,
                Times,
                3
            )
        )
        calculateExpression(calculation) should be (7)

        calculation = Calculation(
            1,
            Plus,
            Calculation(
                2,
                Times,
                Calculation(
                    2,
                    Times,
                    3
                )
            )
        )
        calculateExpression(calculation) should be (13)
    }
}
