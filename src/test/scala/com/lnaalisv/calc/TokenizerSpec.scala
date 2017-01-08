package com.lnaalisv.calc

import org.scalatest._

import Tokenizer._

class TokenizerSpec extends FlatSpec with Matchers {
    "tokenize" should "tokenize 1 + 1" in {
        val tokens : List[Token] = "1 + 1".tokenize
        val correctTokens : List[Token] = List(Number(1), Operator('+'), Number(1))
        tokens should be (correctTokens)
    }

    it should "tokenize empty string" in {
        "".tokenize should be (List())
    }

    it should "tokenize operations +, -, * and /" in {
        var tokens : List[Token] = "1 + 1 + 2".tokenize
        var correctTokens : List[Token] = List(Number(1), Operator('+'), Number(1), Operator('+'), Number(2))
        tokens should be (correctTokens)

        tokens = "1 - 1 - 2".tokenize
        correctTokens = List(Number(1), Operator('-'), Number(1), Operator('-'), Number(2))
        tokens should be (correctTokens)

        tokens = "1 * 1 * 2".tokenize
        correctTokens = List(Number(1), Operator('*'), Number(1), Operator('*'), Number(2))
        tokens should be (correctTokens)

        tokens = "1 / 1 / 2".tokenize
        correctTokens = List(Number(1), Operator('/'), Number(1), Operator('/'), Number(2))
        tokens should be (correctTokens)
    }

    it should "tokenize parenthesis" in {
        val tokens : List[Token] = "(1-1)" tokenize
        val correctTokens : List[Token] = List(
            Parenthesis('('),
            Number(1),
            Operator('-'),
            Number(1),
            Parenthesis(')')
        )
        tokens should be (correctTokens)
    }

    it should "throw when encountering unknown characters" in {
        a [Exception] should be thrownBy {
            "1+a" tokenize
        }
    }

}
