package com.lnaalisv.calc

trait ASTStep
case class Final(rootNode : Expression) extends ASTStep
case class Step(rootNode : Option[Expression], tokens : List[Token]) extends ASTStep
case class ParseError(message : String) extends ASTStep

object ASTStep {
    def convertToFinalIfNeeded(astStep : ASTStep) : ASTStep = {
        astStep match {
            case Step(rootNode, tokens) => if (tokens.isEmpty) Final(rootNode.get) else astStep
            case _ : ASTStep => astStep
        }
    }
}

object Step {
    def apply(rootNode : Expression, tokens : List[Token]) : Step = new Step(Some(rootNode), tokens)
}
