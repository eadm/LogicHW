package ru.nobird.scala.expression.formal

import ru.nobird.scala.expression.{ExpressionType, Expression}
import ru.nobird.scala.expression.ExpressionType.ExpressionType

/**
  * Created by ruslandavletshin on 20/02/16.
  */
class Suc(e: Expression) extends Expression("(" + e.toString + ")'"){
    override def execute(vars: Map[String, Boolean]): Option[Boolean] = e.execute(vars)

    override def getType: ExpressionType = ExpressionType.Suc

    override def getChild: List[Expression] = List(e)

    override def insertExpression(vars: Map[String, Expression]): Expression = new Suc(e.insertExpression(vars))

    override def getListOfVariables: Set[String] = e.getListOfVariables
}
