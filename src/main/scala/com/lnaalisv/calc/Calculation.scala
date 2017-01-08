package com.lnaalisv.calc

case class Calculation(left : Expression, operator : Operator, right : Expression) extends Expression

object Calculation {
    def apply(left: Float, operator: Operator, right: Float): Calculation = new Calculation(Value(left), operator, Value(right))
    def apply(left: Float, operator: Operator, right: Expression): Calculation = new Calculation(Value(left), operator, right)
    def apply(left: Expression, operator: Operator, right: Float): Calculation = new Calculation(left, operator, Value(right))
}
