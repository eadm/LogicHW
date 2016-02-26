package ru.nobird.scala.expression.formal

import ru.nobird.scala.expression.{ExpressionType, Expression}
import ru.nobird.scala.expression.ExpressionType.ExpressionType

/**
  * Created by ruslandavletshin on 20/02/16.
  */
class Add(left: Expression, right: Expression) extends Expression("(" + left.toString + "+" + right.toString + ")"){
    def execute(vars: Map[String, Boolean]): Option[Boolean] = None

    def getType: ExpressionType = ExpressionType.Add
    def getChild: List[Expression] = List(left, right)
    def insertExpression(vars: Map[String, Expression]): Expression = new Add(left.insertExpression(vars), right.insertExpression(vars))
    def getListOfVariables: Set[String] = left.getListOfVariables ++ right.getListOfVariables
}
