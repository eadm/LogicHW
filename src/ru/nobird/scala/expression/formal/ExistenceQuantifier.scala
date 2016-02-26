package ru.nobird.scala.expression.formal

import ru.nobird.scala.expression.{ExpressionType, Expression}
import ru.nobird.scala.expression.ExpressionType.ExpressionType

/**
  * Created by ruslandavletshin on 21/02/16.
  */

class ExistenceQuantifier(v: String, body: Expression) extends Expression("?" + v + "(" + body.toString + ")") {
    override def execute(vars: Map[String, Boolean]): Option[Boolean] = None
    override def getType: ExpressionType = ExpressionType.ExistenceQuantifier

    override def getChild: List[Expression] = List(body)
    override def insertExpression(vars: Map[String, Expression]): Expression = new ExistenceQuantifier(v, body.insertExpression(vars))
    override def getListOfVariables: Set[String] = Set()

    override def getFreeVariables(used: Set[String]) = (Set[String]() /: getChild)((s, c) => s ++ c.getFreeVariables(used + v))
    override def getLinkedVariables = (Set[String](v) /: getChild)((s, c) => s ++ c.getLinkedVariables)
    override def getLinkedVariablesOnWayToX(x: String, current: Set[String]): Set[String] =
        (Set[String]() /: getChild)((s, c) => s ++ c.getLinkedVariablesOnWayToX(x, current + v))
}

