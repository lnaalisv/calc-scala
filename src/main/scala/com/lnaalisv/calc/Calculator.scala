package com.lnaalisv.calc

object Calculator {

    def calculateExpression(e : Expression) : Float = {
        e match {
            case Value(value) => value
            case Calculation(left, operator, right) => operator match {
                case Operator('+') => calculateExpression(left) + calculateExpression(right)
                case Operator('-') => calculateExpression(left) - calculateExpression(right)
                case Operator('*') => calculateExpression(left) * calculateExpression(right)
                case Operator('/') => calculateExpression(left) / calculateExpression(right)
                case Operator(operator) => throw new Exception("Unknown operator: " + operator)
            }
        }
    }
}
