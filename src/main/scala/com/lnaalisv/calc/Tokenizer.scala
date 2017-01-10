package com.lnaalisv.calc

trait Token

case class Parenthesis(paren : Char) extends Token
case class Operator(operator : Char) extends Token
case class Number(number : Float) extends Token

object Tokenizer {

    val parenthesis = "()"
    val operators = "+-*/"

    val Plus = Operator('+')
    val Minus = Operator('-')
    val Multiply = Operator('*')
    val Divide = Operator('/')
    val LeftParenthesis = Parenthesis('(')
    val RightParenthesis = Parenthesis(')')

    type isTokenCharacter = (Char) => Boolean
    def isParenthesisChar : isTokenCharacter = parenthesis.indexOf(_) != -1
    def isOperatorChar : isTokenCharacter = operators.indexOf(_) != -1
    def isOperator(t : Token) : Boolean = t match {
        case Operator(_) => true
        case _ => false
    }
    def isNotWhiteSpace : isTokenCharacter = _ != ' '
    def stackedIsNumber(str : String) : Boolean = {
        try {
            val numeric = str.toFloat
            true
        } catch {
            case _ : Throwable => false
        }
    }
    def toParenthesisOrOperator(c : Char) : Token = {
        if (isParenthesisChar(c)) Parenthesis(c) else Operator(c)
    }

    def tokenToString(t : Token) : String = t match {
        case Parenthesis(paren) => paren.toString
        case Operator(operator) => operator.toString
        case Number(number) => number.toString
    }

    @throws(classOf[Exception])
    def imperativeTokenize(str : String) : List[Token] = {

        var tokens : List[Token] = List()
        var stack : String = ""
        for (c <- str.filter(isNotWhiteSpace)) {
            if (isParenthesisChar(c) || isOperatorChar(c)) {
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

        tokens
    }

    @throws(classOf[Exception])
    def recursiveTokenize(str : String, stack : String = "") : List[Token] = {
        if (str.isEmpty) return List()
        val firstChar = str.head
        val rest = str.tail
        if (isParenthesisChar(firstChar) || isOperatorChar(firstChar)) {
            if (stack.isEmpty) {
                return toParenthesisOrOperator(firstChar) :: recursiveTokenize(rest)
            } else {
                return List(Number(stack.toFloat), toParenthesisOrOperator(firstChar)) ::: recursiveTokenize(rest)
            }
        } else if(stackedIsNumber(stack + firstChar)) {
            if (rest.isEmpty) {
                return List(Number((stack + firstChar).toFloat))
            } else {
                return recursiveTokenize(rest, stack + firstChar)
            }
        }
        throw new Exception("Parse error at " + firstChar)
    }

    implicit class StringTokenizer(str : String) {

        @throws(classOf[Exception])
        def tokenize : List[Token] = {
            try {
                println("Tokenizing " + str)
                val result = recursiveTokenize(str.filter(isNotWhiteSpace))
                println("Tokenizing result " + result.map(tokenToString))
                result
            } catch {
                case ex : Throwable => throw ex
            }
        }

    }
}
