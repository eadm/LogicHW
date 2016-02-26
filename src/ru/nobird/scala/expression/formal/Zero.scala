package ru.nobird.scala.expression.formal

import ru.nobird.scala.expression.{ExpressionType, Expression}
import ru.nobird.scala.expression.ExpressionType.ExpressionType

/**
  * Created by ruslandavletshin on 20/02/16.
  */
class Zero extends Expression("0"){
    override def execute(vars: Map[String, Boolean]): Option[Boolean] = None

    override def getType: ExpressionType = ExpressionType.Zero

    override def getChild: List[Expression] = List()

    override def insertExpression(vars: Map[String, Expression]): Expression = this

    override def getListOfVariables: Set[String] = Set()
}
