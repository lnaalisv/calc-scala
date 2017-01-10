package com.lnaalisv.calc

trait Expression
case class Value(value : Float) extends Expression
case class Calculation(left : Expression, operator : Operator, right : Expression) extends Expression
case class ParenthesisExpression(expression : Expression) extends Expression
case class NegateParenthesisExpression(expression : Expression) extends Expression
case object EmptyExpression extends Expression

object Expression {

}

object Value {
    def apply(numberToken : Number): Value = new Value(numberToken.number)
}

object Calculation {
    def apply(left: Number, operator: Operator, right: Expression): Calculation = new Calculation(Value(left.number), operator, right)
    def apply(left: Expression, operator: Operator, right: Number): Calculation = new Calculation(left, operator, Value(right.number))
    def apply(left: Float, operator: Operator, right: Float): Calculation = new Calculation(Value(left), operator, Value(right))
    def apply(left: Float, operator: Operator, right: Expression): Calculation = new Calculation(Value(left), operator, right)
    def apply(left: Expression, operator: Operator, right: Float): Calculation = new Calculation(left, operator, Value(right))
}
