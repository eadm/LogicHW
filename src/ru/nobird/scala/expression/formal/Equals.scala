package ru.nobird.scala.expression.formal

import ru.nobird.scala.expression.{ExpressionType, Expression}
import ru.nobird.scala.expression.ExpressionType.ExpressionType

/**
  * Created by ruslandavletshin on 21/02/16.
  */
class Equals(left: Expression, right: Expression) extends Expression("(" + left.toString + "=" + right.toString + ")"){
    def execute(vars: Map[String, Boolean]): Option[Boolean] = None

    def getType: ExpressionType = ExpressionType.Equals
    def getChild: List[Expression] = List(left, right)
    def insertExpression(vars: Map[String, Expression]): Expression = new Equals(left.insertExpression(vars), right.insertExpression(vars))
    def getListOfVariables: Set[String] = left.getListOfVariables ++ right.getListOfVariables
}
