package ru.nobird.scala.expression

/**
  * Created by ruslandavletshin on 07/02/16.
  */
class None extends Expression(""){
    override def execute(vars: Map[String, Boolean]): Option[Boolean] = None

    def getType = ExpressionType.None
    def getChild = List()
    def getListOfVariables = Set()

    def insertExpression(vars: Map[String, Expression]) = this
}
