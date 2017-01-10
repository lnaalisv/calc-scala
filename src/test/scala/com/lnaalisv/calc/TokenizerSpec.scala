package com.lnaalisv.calc

import org.scalatest._

import Tokenizer._

class TokenizerSpec extends FlatSpec with Matchers {
    "tokenize" should "tokenize 1 + 1" in {
        val tokens : List[Token] = "1 + 1".tokenize
        val correctTokens : List[Token] = List(Number(1), Plus, Number(1))
        tokens should be (correctTokens)
    }

    it should "tokenize empty string" in {
        "".tokenize should be (List())
    }

    it should "tokenize operations +, -, * and /" in {
        var tokens : List[Token] = "1 + 1 + 2".tokenize
        var correctTokens : List[Token] = List(Number(1), Plus, Number(1), Plus, Number(2))
        tokens should be (correctTokens)

        tokens = "12 - 1 - 2".tokenize
        correctTokens = List(Number(12), Minus, Number(1), Minus, Number(2))
        tokens should be (correctTokens)

        tokens = "1 * 11 * 2".tokenize
        correctTokens = List(Number(1), Multiply, Number(11), Multiply, Number(2))
        tokens should be (correctTokens)

        tokens = "1 / 1 / 200".tokenize
        correctTokens = List(Number(1), Divide, Number(1), Divide, Number(200))
        tokens should be (correctTokens)
    }

    it should "tokenize parenthesis" in {
        val tokens : List[Token] = "(1-1)" tokenize
        val correctTokens : List[Token] = List(
            LeftParenthesis,
            Number(1),
            Minus,
            Number(1),
            RightParenthesis
        )
        tokens should be (correctTokens)
    }

    it should "throw when encountering unknown characters" in {
        a [Exception] should be thrownBy {
            "1+a" tokenize
        }
    }

}
