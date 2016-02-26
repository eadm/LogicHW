package ru.nobird.scala.expression

/**
  * Created by ruslandavletshin on 07/02/16.
  */
class Conjunction (left: Expression, right: Expression) extends Expression(left + "&" + right) {
//    override def toString =  left + " and " + right

    def execute(vars: Map[String, Boolean]): Option[Boolean] =
        (left.execute(vars), right.execute(vars)) match {
            case (Some(a), Some(b)) => new Some[Boolean](a && b)
            case default => None
        }

    def getType = ExpressionType.Conjunction
    def getChild = List(left, right)
    def getListOfVariables = left.getListOfVariables ++ right.getListOfVariables

    def insertExpression(vars: Map[String, Expression]) = new Conjunction(left.insertExpression(vars), right.insertExpression(vars))
}
