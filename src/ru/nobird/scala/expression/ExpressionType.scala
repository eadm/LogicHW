package ru.nobird.scala.expression

/**
  * Created by ruslandavletshin on 08/02/16.
  */
object ExpressionType extends Enumeration {
    type ExpressionType = Value
    val Brackets, Conjunction, Disjunction, Implication, Negation, None, Variable = Value
    val Add, Equals, Mul, Func, Predicate, Suc, Zero = Value
    val UniversalQuantifier, ExistenceQuantifier = Value
}
