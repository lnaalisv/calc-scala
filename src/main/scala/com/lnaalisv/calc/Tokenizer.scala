package com.lnaalisv.calc

trait Token

case class Parenthesis(paren : Char) extends Token
case class Operator(operator : Char) extends Token
case class Number(number : Float) extends Token

object Tokenizer {

    val parenthesis = "()"
    val operators = "+-*/"

    type isTokenCharacter = (Char) => Boolean
    def isParenthesis : isTokenCharacter = parenthesis.indexOf(_) != -1
    def isOperator : isTokenCharacter = operators.indexOf(_) != -1
    def isNotWhiteSpace : isTokenCharacter = _ != ' '
    def stackedIsNumber(str : String) : Boolean = {
        try {
            val numeric = str.toFloat
            return true
        } catch {
            case _ => return false
        }
    }
    def toParenthesisOrOperator(c : Char) : Token = {
        if (isParenthesis(c)) Parenthesis(c) else Operator(c)
    }

    def tokenToString(t : Token) : String = t match {
        case Parenthesis(paren) => paren.toString
        case Operator(operator) => operator.toString
        case Number(number) => number.toString
    }

    implicit class StringTokenizer(str : String) {

        @throws(classOf[Exception])
        def tokenize : List[Token] = {
            println("Tokenizing " + str)

            var tokens : List[Token] = List()
            var stack : String = ""
            for (c <- str.filter(isNotWhiteSpace)) {
                if (isParenthesis(c) || isOperator(c)) {
                    if (!stack.isEmpty) {
                        tokens = tokens :+ Number(stack.toFloat)
                        stack = ""
                    }
                    tokens = tokens :+ toParenthesisOrOperator(c)
                } else if (stackedIsNumber(stack + c)) {
                    stack += c
                } else {
                    throw new Exception("Could not parse: " + c)
                }
            }

            if (!stack.isEmpty) {
                tokens = tokens :+ Number(stack.toFloat)
            }

            println("Tokenizing result: " + tokens.map(tokenToString))

            tokens
        }
    }
}
