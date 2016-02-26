package ru.nobird.scala.parser

import ru.nobird.scala.Util

/**
  * Created by ruslandavletshin on 07/02/16.
  */
class Parser[A](run: String => List[(A, String)]) {
    def apply(s: String) = run(Util.removeWhitespaces(s))

    def flatMap[B](f: A => Parser[B]): Parser[B] = {
        val runB = {s: String => run(s).flatMap({ case (a, rest) => f(a)(rest)})}
        new Parser[B](runB)
    }

    def map[B](f: A => B): Parser[B] = {
        val runB = {s: String => run(s).map({ case (a, rest) => (f(a),rest)})}
        new Parser[B](runB)
    }
}