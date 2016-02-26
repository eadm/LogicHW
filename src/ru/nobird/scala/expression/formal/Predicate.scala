package ru.nobird.scala.expression.formal

import ru.nobird.scala.expression.{ExpressionType, Expression}
import ru.nobird.scala.expression.ExpressionType._

/**
  * Created by ruslandavletshin on 21/02/16.
  */
class Predicate(name: String, args: List[Expression]) extends Func(name, args){
    override def getType: ExpressionType = ExpressionType.Predicate
    override def insertExpression(vars: Map[String, Expression]): Expression = new Predicate(name, args.map(e => e.insertExpression(vars)))

    override def equals(e: Expression): Boolean = {
        if (e.reduceBrackets.getType == ExpressionType.Predicate) {
            e.reduceBrackets.asInstanceOf[Predicate].getName == name && super.equals(e)
        } else super.equals(e)
    }
}
