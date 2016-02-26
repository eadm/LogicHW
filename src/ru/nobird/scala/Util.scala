package ru.nobird.scala

import ru.nobird.scala.expression.{ExpressionType, Expression}

/**
  * Created by ruslandavletshin on 07/02/16.
  */
object Util {
    def removeWhitespaces(s: String): String = s.filterNot(c => c.isWhitespace)

    def createPairedList[A, B](a: List[A], b: List[B]): List[(A, B)] = {
        if (a.isEmpty)
            List()
        else
            (a.head, b.head) :: createPairedList(a.tail, b.tail)
    }

    def escapeBrackets(e: Expression):Expression =
        if (e.getType == ExpressionType.Brackets)
            escapeBrackets(e.getChild.head)
        else
            e
}
