package ru.nobird.scala.expression

/**
  * Created by ruslandavletshin on 07/02/16.
  */
class Negation(x: Expression) extends Expression("!" + x){
//    override def toString = "not " + x

    def execute(vars: Map[String, Boolean]): Option[Boolean] =
        x.execute(vars) match {
            case Some(a) => new Some[Boolean](!a)
            case default => None
        }

    def getType = ExpressionType.Negation
    def getChild = List(x)
    def getListOfVariables = x.getListOfVariables

    def insertExpression(vars: Map[String, Expression]) = new Negation(x.insertExpression(vars))
}
