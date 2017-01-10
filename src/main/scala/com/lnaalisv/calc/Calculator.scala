package com.lnaalisv.calc

import ASTStep._
import Tokenizer._
import Utils._
import Expression._

object Calculator {
    // ast level steps
    def replaceRootNode(rootNode : Calculation, valueExpression : Expression, operatorToken : Operator) : Expression = {
        Calculation(
            Calculation(
                rootNode.left,
                rootNode.operator,
                rootNode.right match {
                    case Calculation(l : Expression, o : Operator, _) => Calculation(l, o, valueExpression)
                    case _ => valueExpression
                }
            ),
            operatorToken,
            EmptyExpression
        )
    }

    def replaceRightMostNode(rootNode : Calculation, valueExpression : Expression, operatorToken : Operator) : Expression = {
        Calculation(
            rootNode.left,
            rootNode.operator,
            rootNode.right match {
                case Calculation(l, o, _) => Calculation(
                    Calculation(l, o, valueExpression),
                    operatorToken,
                    EmptyExpression
                )
                case _ => Calculation(valueExpression, operatorToken, EmptyExpression)
            }
        )
    }

    def addCalculationToRootNode(rootNode : Calculation, valueExpression : Expression, operatorToken : Operator) : Expression = {
        operatorToken match {
            case (Plus | Minus) => replaceRootNode(rootNode, valueExpression, operatorToken)
            case Multiply => operatorToken match {
                case Divide => replaceRootNode(rootNode, valueExpression, operatorToken)
                case _ => replaceRightMostNode(rootNode, valueExpression, operatorToken)
            }
            case Divide => rootNode.operator match {
                case (Multiply | Divide) => replaceRootNode(rootNode, valueExpression, operatorToken)
                case _ => replaceRightMostNode(rootNode, valueExpression, operatorToken)
            }
            case Operator(operator) => throw new Exception("Operator " + operator + " not implemented")
        }
    }

    // substeps
    def calculationStep(rootNode : Option[Expression], valueExpression : Expression, operatorToken : Token, restTokens : List[Token]): ASTStep = {
        operatorToken match {
            case op @ Operator(_) => rootNode match {
                case Some(expression) => expression match {
                    case Calculation(l, o, r) => Step( addCalculationToRootNode(Calculation(l, o, r), valueExpression, op), restTokens)
                    case _ => ParseError("Unknown root node")
                }
                case None => Step(Calculation(valueExpression, op, EmptyExpression), restTokens)
            }
            case _ => ParseError("Token is not operator " + tokenToString(operatorToken))
        }
    }

    def finalizeToRootNodeStep(rootNode : Calculation, newRight : Expression) : Step = {
        Step(Calculation(
            rootNode.left,
            rootNode.operator,
            rootNode.right match {
                case EmptyExpression => newRight
                case Calculation(l, o, _) => Calculation(l, o, newRight)
            }
        ), List())
    }

    def finalStep(rootNode : Option[Expression], finalExpression : Expression) : Step = {
        rootNode match {
            case Some(expression) => finalizeToRootNodeStep(expression.asInstanceOf[Calculation], finalExpression)
            case None => Step(Some(finalExpression), List())
        }
    }

    // main level steps
    def numberOrOperatorStep(rootNode : Option[Expression], numberToken : Number, operatorToken : Option[Token], restTokens : List[Token]): ASTStep = {
        operatorToken match {
            case Some(operator) => calculationStep(rootNode, Value(numberToken), operator, restTokens)
            case None => finalStep(rootNode, Value(numberToken))
        }
    }

    type parenthesisConstructor = (Expression) => Expression
    def parenthesisStep(rootNode : Option[Expression], parenthesisCons : parenthesisConstructor, tokens : List[Token]) : ASTStep = {
        val matchingIndex = getMatchingParenthesisIndex(tokens)
        if (matchingIndex == -1) {
            return ParseError("Matching parentheses not found")
        } else if (matchingIndex == 0) {
            return ParseError("Empty parentheses found")
        }

        val (subTokens, remainingTokens) = tokens.splitAt(matchingIndex)
        val parenthesisExpression = parenthesisCons(tokensToAST(subTokens))

        if (remainingTokens.length == 1) {
            // only RightParenthesis left
            return rootNode match {
                case Some(_) => finalStep(rootNode, parenthesisExpression)
                case None => Step(parenthesisExpression, List())
            }
        }

        calculationStep(rootNode, parenthesisExpression, remainingTokens(1), remainingTokens.takeRight(remainingTokens.length - 2))
    }

    def negateStep(rootNode : Option[Expression], firstToken : Option[Token], secondToken : Option[Token], restTokens : List[Token]) : ASTStep = {
        firstToken match {
            case Some(maybeNum) => maybeNum match {
                case Number(number) => numberOrOperatorStep(rootNode, Number(-number), secondToken, restTokens)
                case LeftParenthesis => parenthesisStep(rootNode, NegateParenthesisExpression, restTokens.prependOption(secondToken))
                case _ => null
            }
            case None => ParseError("Invalid minus token")
        }
    }

    def processASTStep(step : Step) : ASTStep = {
        val rootNode = step.rootNode
        val firstToken = step.tokens.lift(0)
        val secondToken = step.tokens.lift(1)
        val thirdToken = step.tokens.lift(2)
        val restTokens = step.tokens.takeRight(step.tokens.length - 3)
        firstToken match {
            case Some(token) => token match {
                case Minus => negateStep(rootNode, secondToken, thirdToken, restTokens)
                case num @ Number(_) => numberOrOperatorStep(rootNode, num, secondToken, restTokens.prependOption(thirdToken))
                case LeftParenthesis => parenthesisStep(rootNode, ParenthesisExpression, restTokens.prependOption(thirdToken).prependOption(secondToken))
                case _ => ParseError("Invalid first token")
            }
            case None => Final(EmptyExpression)
        }
    }

    def processASTSteps(astStep : ASTStep) : ASTStep = {
        astStep match {
            case step @ Step(_, _) => processASTSteps(convertToFinalIfNeeded(processASTStep(step)))
            case Final(_) => astStep
            case ParseError(_) => astStep
        }
    }

    @throws(classOf[Exception])
    def tokensToAST(tokens : List[Token]) : Expression = {
        val finalStep = processASTSteps(Step(None, tokens))
        finalStep match {
            case Final(expression) => expression
            case ParseError(message) => throw new Exception("Parse error: " + message)
        }
    }

    def calculateExpression(e : Expression) : Float = {
        e match {
            case Value(value) => value
            case ParenthesisExpression(expression) => calculateExpression(expression)
            case NegateParenthesisExpression(expression) => -calculateExpression(expression)
            case Calculation(left, operator, right) => operator match {
                case Plus => calculateExpression(left) + calculateExpression(right)
                case Minus => calculateExpression(left) - calculateExpression(right)
                case Multiply => calculateExpression(left) * calculateExpression(right)
                case Divide => calculateExpression(left) / calculateExpression(right)
                case Operator(op) => throw new Exception("Unknown operator: " + op)
            }
        }
    }

    def calculate(str : String) : Float = {
        //println("Calculating " + str)
        val ast = tokensToAST(str.tokenize)
        //println("AST: " + expressionToString(ast))
        val result = calculateExpression(ast)
        //println("Result is " + result.toString)
        result
    }
}
