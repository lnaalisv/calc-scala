package com.lnaalisv.calc

import Tokenizer._

object Calc {
    def main(args: Array[String]) {
        if(args.length == 1) {
            try {
                "(1 +1  )a" tokenize;
                val calculation: String = args(0)
                println("Result is " + calculation)
            } catch {
                case e : Exception => println(e.toString)
            }
        } else {
            println("No arguments given, usage e.g.  calc \"1+1\"")
        }
    }
}
