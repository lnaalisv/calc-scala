package com.lnaalisv.calc

trait ASTStep
case class Final(rootNode : Expression) extends ASTStep
case class Step(rootNode : Expression, tokens : List[Token]) extends ASTStep
case class Error(message : String) extends ASTStep

object ASTStep {

}
