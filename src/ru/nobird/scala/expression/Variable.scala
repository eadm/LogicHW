package ru.nobird.scala.expression


/**
  * Created by ruslandavletshin on 07/02/16.
  */
class Variable(x: String) extends Expression(x) {
    def execute(vars: Map[String, Boolean]): Option[Boolean] = vars.get(x)

    def getType = ExpressionType.Variable
    def getChild = List()
    def getListOfVariables = Set(x)

    def insertExpression(vars: Map[String, Expression]) = vars.get(x) match {
        case Some(xx) => xx
        case None => this
    }

    override def getFreeVariables(used: Set[String]) = if (used(x)) Set() else Set(x)
    override def getLinkedVariablesOnWayToX(x: String, current: Set[String]) = if (x == this.x) current else Set()
}