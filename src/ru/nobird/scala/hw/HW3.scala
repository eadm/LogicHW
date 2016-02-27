package ru.nobird.scala.hw

import java.io.FileWriter

import ru.nobird.scala.Reader
import ru.nobird.scala.expression._
import ru.nobird.scala.logic.{Completeness, Classic}
import ru.nobird.scala.parser.ClassicMonadParser

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by ruslandavletshin on 11/02/16.
  */
object HW3 extends HW {

    def execute(reader: Reader, writer: FileWriter): (Boolean, String) = {
        val parser = ClassicMonadParser.createExpressionParser()
        val target = parser(reader.readLine()).head._1 // target expression
        val variables = target.getListOfVariables.toList // list of variables in expression

        var wrong = false
        for (
            vv <- genValues(variables, Map()) if !wrong
        ) target.execute(vv) match {
                case Some(false) =>
                    println("Высказывание ложно при" + vv.foldLeft("")((s, v) => s + " " + v._1 + "=" + (if(v._2) "И" else "Л") ))
                    writer.write("Высказывание ложно при" + vv.foldLeft("")((s, v) => s + " " + v._1 + "=" + (if(v._2) "И" else "Л") ))
                    wrong = true

                case default =>
            }

        if (!wrong) {
            println("|=" + target.toString)
            writer.write("|-" + target.toString + "\n")

            for (l <- genProof(variables, List(), target))
                writer.write(l.toString + "\n")
        }
        (true, "")
    }

    def genProof(variables: List[String], current: List[Expression], target: Expression): Vector[Expression] = {
        variables match {
            case vx::vs => // объединение переменных
                val v = new Variable(vx)
                HW2.deduction(Classic.getAxioms, current :+ v, genProof(vs, current :+ v, target)).toVector ++
                HW2.deduction(Classic.getAxioms, current :+ new Negation(v), genProof(vs, current :+ new Negation(v), target)).toVector ++
                toBeOrNotToBe(Classic.getAxioms, v) ++
                Vector(
                    new Implication(
                        new Brackets(new Implication(v, target)),
                        new Implication(
                            new Brackets(new Implication(new Negation(v), target)),
                            new Brackets(new Implication(
                                new Brackets(new Disjunction(v, new Negation(v))),
                                target
                            ))
                        )
                    ),
                    new Implication(
                        new Brackets(new Implication(new Negation(v), target)),
                        new Brackets(new Implication(
                            new Brackets(new Disjunction(v, new Negation(v))),
                            target
                        ))
                    ),
                    new Implication(
                        new Brackets(new Disjunction(v, new Negation(v))),
                        target
                    ),
                    target
                )
            case List() => proveBunch(target.getBunches, valuesFromVariables(current), mutable.Set())
        }
    }

    def proveBunch(
                      bunches: List[Expression],
                      values: Map[String, Boolean],
                      proved: mutable.Set[Expression]
                  ): Vector[Expression] = bunches match {
        case b :: bs =>
            if (proved(b)) {
                proveBunch(bs, values, proved)
            } else {
                Completeness.proveBunch(b, values) ++ proveBunch(bs, values, proved += b)
            }
        case List() => Vector()
    }

    def valuesFromVariables(variables: List[Expression]): Map[String, Boolean] = variables match { // делает из списка вида A,!B оценки вида A->true, B->false
        case v::vs => valuesFromVariables(vs) ++ (if (v.getType == ExpressionType.Negation) Map(v.getChild.head.toString -> false) else Map(v.toString -> true))
        case List() => Map[String, Boolean]()
    }



    def genValues(variables: List[String], current: Map[String, Boolean]): List[Map[String, Boolean]] = // generates maps of values
        variables match {
            case v :: vs => genValues(vs, current ++ Map[String, Boolean](v -> false)) ++ genValues(vs, current ++ Map[String, Boolean](v -> true))
            case List() => List(current)
        }

    def toBeOrNotToBe(axioms: List[Expression], pp:Expression): ArrayBuffer[Expression] = {
        val p = if (pp.getType == ExpressionType.Variable || pp.getType == ExpressionType.Brackets) pp else new Brackets(pp)

        val proof = ArrayBuffer[Expression](new Implication(p, new Disjunction(p, new Negation(p))))
        proof ++= counterPos(axioms, p, new Disjunction(p, new Negation(p)))
        proof += new Implication(new Negation(new Brackets(new Disjunction(p, new Negation(p)))), new Negation(p)) //p1

        proof += new Implication(new Negation(p), new Disjunction(p, new Negation(p)))
        proof ++= counterPos(axioms, new Negation(p), new Disjunction(p, new Negation(p)))
        proof += new Implication(new Negation(new Brackets(new Disjunction(p, new Negation(p)))), new Negation(new Negation(p))) // p2

        proof +=
            new Implication(new Brackets(new Implication(new Negation(new Brackets(new Disjunction(p, new Negation(p)))), new Negation(p))),
            new Implication(new Brackets(new Implication(new Negation(new Brackets(new Disjunction(p, new Negation(p)))), new Negation(new Negation(p)))),
            new Brackets(new Negation(new Negation(new Brackets(new Disjunction(p, new Negation(p)))))))) // 9 aks

        proof +=
            new Implication(new Brackets(new Implication(new Negation(new Brackets(new Disjunction(p, new Negation(p)))), new Negation(new Negation(p)))),
            new Brackets(new Negation(new Negation(new Brackets(new Disjunction(p, new Negation(p))))))) //m1 1.3

        proof += new Negation(new Negation(new Brackets(new Disjunction(p, new Negation(p))))) // mp 2,4
        proof += new Implication(
            new Negation(new Negation(new Brackets(new Disjunction(p, new Negation(p))))),
            new Brackets(new Disjunction(p, new Negation(p)))
        )
        proof += new Disjunction(p, new Negation(p))
    }

    def counterPos(axioms: List[Expression], aa:Expression, bb:Expression): ArrayBuffer[Expression] = {
        val a = new Brackets(aa)
        val b = new Brackets(bb)
        HW2.deduction(
            axioms,
            List(new Implication(a, b)),
            HW2.deduction(
                axioms,
                List(
                    new Implication(a, b),
                    new Negation(b)
                ),
                ArrayBuffer(
                    new Implication(
                        new Brackets(new Implication(a, b)),
                        new Implication(new Brackets(new Implication(a, new Negation(b))), new Negation(a))
                    ),
                    new Implication(a, b),
                    new Implication(new Brackets(new Implication(a, new Negation(b))), new Negation(a)),
                    new Implication(new Negation(b), new Implication(a, new Negation(b))),
                    new Negation(b),
                    new Implication(a, new Negation(b)),
                    new Negation(a)
                )
            )
        )
    }
}
