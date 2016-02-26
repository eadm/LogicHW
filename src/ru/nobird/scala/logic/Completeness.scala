package ru.nobird.scala.logic

import ru.nobird.scala.expression._
import ru.nobird.scala.hw.HW2

import scala.collection.mutable.ArrayBuffer

/**
  * Created by ruslandavletshin on 18/02/16.
  */
object Completeness {
    def proveBunch(e: Expression, values: Map[String, Boolean]): Vector[Expression] = e.getType match {
        case ExpressionType.Implication => proveImplication(e, values)
        case ExpressionType.Conjunction => proveConjunction(e, values)
        case ExpressionType.Disjunction => proveDisjunction(e, values)
        case ExpressionType.Negation    => proveNegation(e, values)
        case default => Vector()
    }

    private def proveNegation(ee: Expression, values: Map[String, Boolean]): Vector[Expression] = {
        val a = new Brackets(ee.getChild.head)
        val e = new Brackets(ee)

        (a.execute(values), e.execute(values)) match {
            case (Some(false), Some(true)) => // !A|-!A
                Vector(e)

            case default => // A|-!!A
                Vector(
                    a, //0
                    new Implication(a, new Implication(new Negation(a), a)), //1
                    new Implication(new Negation(a), a), //2
                    new Implication(new Negation(a), new Implication(new Negation(a), new Negation(a))),//3
                    new Implication(//4
                        new Brackets(new Implication(new Negation(a), new Implication(new Negation(a), new Negation(a)))),
                        new Implication(
                            new Brackets(
                                new Implication(
                                    new Negation(a),
                                    new Brackets(
                                        new Implication(
                                            new Brackets(new Implication(new Negation(a), new Negation(a))),
                                            new Negation(a)
                                        )
                                    )
                                )
                            ),
                            new Brackets(new Implication(new Negation(a), new Negation(a)))
                        )
                    ),
                    new Implication(
                        new Brackets(
                            new Implication(
                                new Negation(a),
                                new Brackets(
                                    new Implication(
                                        new Brackets(new Implication(new Negation(a), new Negation(a))),
                                        new Negation(a)
                                    )
                                )
                            )
                        ),
                        new Brackets(new Implication(new Negation(a), new Negation(a)))
                    ),
                    new Implication(
                        new Negation(a),
                        new Implication(
                            new Brackets(new Implication(new Negation(a), new Negation(a))),
                            new Negation(a)
                        )
                    ),
                    new Implication(new Negation(a), new Negation(a)),
                    new Implication(
                        new Brackets(new Implication(new Negation(a), a)),
                        new Implication(
                            new Brackets(new Implication(new Negation(a), new Negation(a))),
                            new Negation(new Negation(a))
                        )
                    ),
                    new Implication(
                        new Brackets(new Implication(new Negation(a), new Negation(a))),
                        new Negation(new Negation(a))
                    ),
                    new Negation(new Negation(a))
                )

        }
    }

    private def worstCaseImplication(a: Expression, b: Expression, e: Expression): Vector[Expression] = {
        HW2.deduction( // !A,!B,A|-B
            Classic.getAxioms,
            List(
                new Negation(a),
                new Negation(b),
                a
            ),
            ArrayBuffer(
                new Implication(
                    new Brackets(
                        new Implication(
                            new Negation(b),
                            a
                        )
                    ),
                    new Implication(
                        new Brackets(
                            new Implication(
                                new Negation(b),
                                new Negation(a)
                            )
                        ),
                        new Negation(new Brackets(new Negation(b)))
                    )
                ),
                a,
                new Implication(a, new Implication(new Negation(b), a)),
                new Implication(new Negation(b), a),
                new Implication(
                    new Brackets(
                        new Implication(
                            new Negation(b),
                            new Negation(a)
                        )
                    ),
                    new Negation(new Brackets(new Negation(b)))
                ),
                new Negation(a),
                new Implication(new Negation(a), new Implication(new Negation(b), new Negation(a))),
                new Implication(new Negation(b), new Negation(a)),
                new Negation(new Brackets(new Negation(b))),
                new Implication(new Negation(new Brackets(new Negation(b))), b),
                b
            )
        ).toVector
    }

    private def proveImplication(ee: Expression, values: Map[String, Boolean]): Vector[Expression] = {
        val a = new Brackets(ee.getChild.head)
        val b = new Brackets(ee.getChild.last)
        val e = new Brackets(ee)

        (a.execute(values), b.execute(values), e.execute(values)) match {
            case (Some(false), Some(false), Some(true)) => // !A,!B|-A->B
                worstCaseImplication(a, b, e)

            case (Some(true), Some(false), Some(false)) => // A,!B|-!(A->B)
                Vector(
                    a,
                    new Negation(b),
                    new Implication(new Negation(b), new Implication(e, new Negation(b))),
                    new Negation(b), new Implication(e, new Negation(b))
                ) ++
                HW2.deduction( // A,!B,A->B|-B
                    Classic.getAxioms,
                    List(
                        a,
                        new Negation(b),
                        e
                    ),
                    Vector(
                        a,
                        e,
                        b
                    )
                ).toVector ++
                Vector(
                    new Implication(
                        new Brackets(new Implication(e, b)),
                        new Implication(
                            new Brackets(new Implication(e, new Negation(b))),
                            new Negation(e)
                        )
                    ),
                    new Implication(
                        new Brackets(new Implication(e, new Negation(b))),
                        new Negation(e)
                    ),
                    new Implication(new Negation(b), new Implication(e, new Negation(b))),
                    new Implication(e, new Negation(b)),
                    new Negation(e)
                )
            case default =>  // A,B|-A->B  and   !A,B|-A->B
                Vector(
                    b,
                    new Implication(b, e),
                    e
                )
        }
    }

    private def proveDisjunction(ee: Expression, values: Map[String, Boolean]): Vector[Expression] = {
        val a = new Brackets(ee.getChild.head)
        val b = new Brackets(ee.getChild.last)
        val e = new Brackets(ee)

        (a.execute(values), b.execute(values), e.execute(values)) match {
            case (Some(false), Some(false), Some(false)) =>  // !A,!B|-!(A|B)
                val xxx = ArrayBuffer(new Negation(a), new Negation(b), e)

                HW2.fromAtoA(xxx, a)
                xxx += new Implication(a, a)

                Vector(
                    new Negation(a),
                    new Negation(b),
                    new Implication(
                        new Brackets(new Implication(e, a)),
                        new Implication(
                            new Brackets(new Implication(e, new Negation(a))),
                            new Negation(e)
                        )
                    ),
                    new Implication(
                        new Negation(a),
                        new Implication(e, new Negation(a))
                    ),
                    new Implication(e, new Negation(a))
                ) ++
                HW2.deduction(
                    Classic.getAxioms,
                    List(
                        new Negation(a),
                        new Negation(b),
                        e
                    ),
                    xxx ++
                    worstCaseImplication(b, a, new Brackets(new Implication(b, a))) ++
                    ArrayBuffer(
                        new Brackets(new Implication(b, a)),
                        new Implication(
                            new Brackets(new Implication(a, a)),
                            new Brackets(new Implication(
                                new Brackets(new Implication(b, a)),
                                new Brackets(new Implication(e, a))
                            ))
                        ),
                        new Implication(
                            new Brackets(new Implication(b, a)),
                            new Brackets(new Implication(e, a))
                        ),
                        new Implication(e, a),
                        a
                    )
                ).toVector ++
                Vector(
                    new Implication(e, a),
                    new Implication(new Brackets(new Implication(e, new Negation(a))), new Negation(e)),
                    new Negation(e)
                )

            case (Some(false), Some(true), Some(true)) => // !A,B|-A|B
                Vector(
                    b,
                    new Implication(b, e),
                    e
                )
            case default => // A,B|-A|B  and  A,!B|-A|B
                Vector(
                    a,
                    new Implication(a, e),
                    e
                )
        }
    }

    private def proveConjunction(ee: Expression, values: Map[String, Boolean]): Vector[Expression] = {
        val a = new Brackets(ee.getChild.head)
        val b = new Brackets(ee.getChild.last)
        val e = new Brackets(ee)

        (a.execute(values), b.execute(values), e.execute(values)) match {
            case (Some(true), Some(true), Some(true)) => // A,B|-A&B
                Vector(
                    a,
                    b,
                    new Implication(a, new Implication(b, ee)),
                    new Implication(b, ee),
                    ee
                )

            case (Some(true), Some(false), Some(false)) => // A,!B|-!(A&B)
                Vector(
                    new Negation(b),
                    new Implication(new Brackets(new Implication(e, b)), new Implication(new Brackets(new Implication(e, new Negation(b))), new Negation(e))),
                    new Implication(e, b),
                    new Implication(new Brackets(new Implication(e, new Negation(b))), new Negation(e)),
                    new Implication(new Negation(b), new Implication(e, new Negation(b))),
                    new Implication(e, new Negation(b)),
                    new Negation(e)
                )
            case default => // !A,B|-!(A&B)  and  !A,!B|-!(A&B)
                Vector(
                    new Negation(a),
                    new Implication(new Brackets(new Implication(e, a)), new Implication(new Brackets(new Implication(e, new Negation(a))), new Negation(e))),
                    new Implication(e, a),
                    new Implication(new Brackets(new Implication(e, new Negation(a))), new Negation(e)),
                    new Implication(new Negation(a), new Implication(e, new Negation(a))),
                    new Implication(e, new Negation(a)),
                    new Negation(e)
                )
        }
    }
}
