package ru.nobird.scala.expression.formal

import ru.nobird.scala.expression.{ExpressionType, Expression}
import ru.nobird.scala.expression.ExpressionType.ExpressionType

/**
  * Created by ruslandavletshin on 20/02/16.
  */
class Func(name: String, args: List[Expression])
    extends Expression(name +
        (if (args.nonEmpty)
            "(" + args.head + args.tail.foldLeft("")((s, e) => s + ',' + e.toString) + ")" else "")){

    override def execute(vars: Map[String, Boolean]): Option[Boolean] = None

    override def getType: ExpressionType = ExpressionType.Func

    override def getChild: List[Expression] = args

    override def insertExpression(vars: Map[String, Expression]): Expression = new Func(name, args.map(e => e.insertExpression(vars)))

    override def getListOfVariables: Set[String] = Set() //args.foldLeft(Set())((s, e) => s ++ Set(e.getListOfVariables))

    def getName = name

    override def equals(e: Expression): Boolean = {
        if (e.reduceBrackets.getType == ExpressionType.Func) {
            e.reduceBrackets.asInstanceOf[Func].getName == name && super.equals(e)
        } else super.equals(e)
    }
}
