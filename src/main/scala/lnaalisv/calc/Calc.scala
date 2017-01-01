package main.scala.lnaalisv.calc

import Tokenizer._

object Calc {
  def main(args: Array[String]) {
    if(args.length == 1) {
      val calculation: String = args(0)
      println("Result is " + calculation tokenize)
    } else {
      println("No arguments given, usage e.g.  calc \"1+1\"")
    }
  }
}
