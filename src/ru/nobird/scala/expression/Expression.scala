package ru.nobird.scala.expression

import ru.nobird.scala.Util
import ru.nobird.scala.expression.ExpressionType.ExpressionType

/**
  * Created by ruslandavletshin on 07/02/16.
  */
abstract class Expression(x: String) {
    override def toString = x

    def length = x.length

    def execute(vars: Map[String, Boolean]): Option[Boolean]
    def getType: ExpressionType
    def getChild: List[Expression]
    def getListOfVariables: Set[String]

    def insertExpression(vars: Map[String, Expression]): Expression

    def reduceBrackets: Expression = if (getType == ExpressionType.Brackets) getChild.head.reduceBrackets else this

    def getBunches: List[Expression] = {
        if (getType == ExpressionType.Variable)
            List()
        else
            getChild.foldLeft(List[Expression]())(
                (list, e) => e.reduceBrackets.getBunches ++ list
            ) :+ this
    }

    def equals(e: Expression): Boolean = {
        if (e.getType == ExpressionType.Brackets && getType != ExpressionType.Brackets) {
            equals(e.getChild.head)
        } else
        if (e.getType != ExpressionType.Brackets && getType == ExpressionType.Brackets) {
            getChild.head.equals(e)
        } else
        if (e.getType == getType) {
            if (getType == ExpressionType.Variable) {
                toString == e.toString
            } else {
                Util.createPairedList(getChild, e.getChild).forall(p => p._1.equals(p._2))
            }
        } else false
    }

    def getFreeVariables(used: Set[String]): Set[String] = (Set[String]() /: getChild)((s, c) => s ++ c.getFreeVariables(used))
    def getLinkedVariables: Set[String] = (Set[String]() /: getChild)((s, c) => s ++ c.getLinkedVariables)
    def getLinkedVariablesOnWayToX(x: String, current: Set[String]): Set[String] = (Set[String]() /: getChild)((s, c) => s ++ c.getLinkedVariablesOnWayToX(x, current))
}