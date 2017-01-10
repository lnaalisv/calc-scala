package com.lnaalisv.calc

import Calculator._

object Calc {
    def main(args: Array[String]) {
        if(args.length == 1) {
            try {
                val calculation: String = args(0)
                println("Result is " + calculate(calculation).toString())
            } catch {
                case e : Exception => println(e.toString)
            }
        } else {
            println("No arguments given, usage e.g.  calc \"1+1\"")
        }
    }
}
