package com.lnaalisv.calc

import ASTStep._
import Tokenizer._
import Utils._
import Expression._

object Calculator {
    // ast level steps
    def replaceRootNode(rootNode : Calculation, numberToken : Number, operatorToken : Operator) : Expression = {
        Calculation(
            Calculation(
                rootNode.left,
                rootNode.operator,
                rootNode.right match {
                    case Calculation(l : Expression, o : Operator, _) => Calculation(l, o, numberToken)
                    case _ => Value(numberToken.number)
                }
            ),
            operatorToken,
            EmptyExpression
        )
    }

    def addCalculationToRootNode(rootNode : Calculation, numberToken : Number, operatorToken : Operator) : Expression = {
        operatorToken match {
            case Plus => replaceRootNode(rootNode, numberToken, operatorToken)
            case Minus => replaceRootNode(rootNode, numberToken, operatorToken)
            case Operator(operator) => throw new Exception("Operator " + operator + " not implemented")
        }
    }

    // substeps
    def calculationStep(rootNode : Option[Expression], numberToken : Number, operatorToken : Token, restTokens : List[Token]): ASTStep = {
        operatorToken match {
            case Operator(operator) => rootNode match {
                case Some(expression) => expression match {
                    case Calculation(l, o, r) => Step( addCalculationToRootNode(Calculation(l, o, r), numberToken, Operator(operator)), restTokens)
                    case _ => ParseError("Unknown root node")
                }
                case None => Step(Calculation(numberToken, Operator(operator), EmptyExpression), restTokens)
            }
            case _ => ParseError("Token is not operator " + tokenToString(operatorToken))
        }
    }

    def finalizeToRootNodeStep(rootNode : Calculation, numberToken : Number) : Step = {
        Step(Calculation(
            rootNode.left,
            rootNode.operator,
            rootNode.right match {
                case EmptyExpression => Value(numberToken.number)
                case Calculation(l, o, _) => Calculation(l, o, numberToken)
            }
        ), List())
    }

    def finalStep(rootNode : Option[Expression], numberToken : Number) : Step = {
        rootNode match {
            case Some(expression) => finalizeToRootNodeStep(expression.asInstanceOf[Calculation], numberToken)
            case None => Step(Some(Value(numberToken)), List())
        }
    }

    // main level steps
    def numberOrOperatorStep(rootNode : Option[Expression], numberToken : Number, operatorToken : Option[Token], restTokens : List[Token]): ASTStep = {
        operatorToken match {
            case Some(operator) => calculationStep(rootNode, numberToken, operator, restTokens)
            case None => finalStep(rootNode, numberToken)
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
                case Number(number) => numberOrOperatorStep(rootNode, Number(number), secondToken, restTokens.prependOption(thirdToken))
            }
            case None => throw new Exception("Parse error")
        }
    }

    def processASTSteps(astStep : ASTStep) : ASTStep = {
        astStep match {
            case Final(_) => astStep
            case ParseError(_) => astStep
            case Step(_, _) => processASTSteps(convertToFinalIfNeeded(processASTStep(astStep.asInstanceOf[Step])))
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
        calculateExpression(tokensToAST(str.tokenize))
    }
}
