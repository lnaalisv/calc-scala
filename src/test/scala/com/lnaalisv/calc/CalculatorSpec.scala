package com.lnaalisv.calc

import org.scalatest._
import Calculator._
import Tokenizer._

class CalculatorSpec extends FlatSpec with Matchers {

    "calculateExpression" should "calculate simple calculations" in {
        var calculation = Calculation(1, Plus, 1)
        calculateExpression(calculation) should be (2)

        calculation = Calculation(1, Minus, 1)
        calculateExpression(calculation) should be (0)

        calculation = Calculation(2, Multiply, 3)
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
                Multiply,
                3
            )
        )
        calculateExpression(calculation) should be (7)

        calculation = Calculation(
            1,
            Plus,
            Calculation(
                2,
                Multiply,
                Calculation(
                    2,
                    Multiply,
                    3
                )
            )
        )
        calculateExpression(calculation) should be (13)
    }

    it should "calculate parenthesis" in {
        val calculation = Calculation(
            1,
            Minus,
            ParenthesisExpression(Calculation(
                2,
                Minus,
                1
            ))
        )

        calculateExpression(calculation) should be (0)
    }

    it should "calculate negated parenthesis" in {
        val calculation = Calculation(
            1,
            Minus,
            NegateParenthesisExpression(Calculation(
                2,
                Minus,
                1
            ))
        )

        calculateExpression(calculation) should be (2)
    }

    "calculate" should "calculate simple + expressions" in {
        var result = calculate("1 + 1")
        result should be (2)

        result = calculate("10 + 20")
        result should be (30)

        result = calculate("1 + 2 + 3+ 4")
        result should be (10)
    }

    it should "calculate simple - expressions" in {
        var result = calculate("3 - 1")
        result should be (2)

        result = calculate("1 - 10 - 2")
        result should be (-11)
    }

    it should "calculate simple * expressions" in {
        var result = calculate("8*8")
        result should be (64)

        result = calculate("2*2*2*2*2")
        result should be (32)

        result = calculate("1+ 2*2*2*2*2")
        result should be (33)

        result = calculate("1+ 2*2*2*2*2 + 1")
        result should be (34)
    }

    it should "calculate simple / expressions" in {
        var result = calculate("8/2")
        result should be (4)

        result = calculate("1+8/2")
        result should be (5)
    }

    it should "calculate parenthesis expressions" in {
        var result = calculate("(1 + 1 )")
        result should be (2)

        result = calculate("1 * ( 1 + 1 )")
        result should be (2)
    }

    it should "calculate negation expressions" in {
        var result = calculate("-(1 + 1 )")
        result should be (-2)

        result = calculate("-1 * ( 1 + 1 )")
        result should be (-2)

        result = calculate("1 * -( 1 + 1 )")
        result should be (-2)
    }

}
