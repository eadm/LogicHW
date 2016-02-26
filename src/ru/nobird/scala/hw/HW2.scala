package ru.nobird.scala.hw

import java.io.FileWriter

import ru.nobird.scala.Reader
import ru.nobird.scala.expression.{Expression, Brackets, Implication}
import ru.nobird.scala.logic.Classic
import ru.nobird.scala.parser.ClassicMonadParser

import scala.collection.mutable.ArrayBuffer

/**
  * Created by ruslandavletshin on 10/02/16.
  */
object HW2 extends HW{

    def execute(reader: Reader, writer: FileWriter):(Boolean, String) = {
        val headerParser = ClassicMonadParser.createHeaderParser()
        val parser = ClassicMonadParser.createExpressionParser()
        val axioms = Classic.schemas()

        val (beta :: assumptions) = headerParser(reader.readLine()).head._1
        val alpha = if (assumptions.last.getChild.length > 1) new Brackets(assumptions.last) else assumptions.last

        writer.write(
            (if (assumptions.length > 1)
                assumptions.tail.init.foldLeft(assumptions.head.toString)((s: String, e) => s + "," + e)
            else "")
                + "|-" + new Implication(alpha, beta) + "\n")

        val proof = ArrayBuffer[Expression]()

        while (reader.ready()) {
            val s = reader.readLine()
            parser(s) match {
                case a :: as =>
                    var proved = false
                    if (Classic.checkAxioms(axioms, a._1) != -1 || Classic.checkAssumption(assumptions.init, a._1) != -1) {
                        proved = true
//                        writer.write("Axiom or assumption\n")
                        writer.write(a._1 + "\n")
                        writer.write(new Implication(new Brackets(a._1), new Brackets(new Implication(alpha, a._1))) + "\n")
                    }

                    if (!proved && a._1.equals(alpha)) {
                        proved = true
//                        writer.write("Alpha\n")
                        fromAtoA(writer, alpha)
                    }

                    val (x, y) = if (!proved) Classic.checkModusPonens(proof, a._1) else (-1, -1)
                    if (x != -1) {
//                        writer.write("Modus: " + x + " - " + y +"\n")
                        val dj = proof(x)
                        val dk = proof(y)
                        modus(writer, alpha, dj, dk, a._1)
                    }

                    proof += a._1
                    writer.write(new Implication(alpha, a._1).toString + "\n")
                case default => println("Error: " + s)
            }

        }
        (true, "")
    }

    def deduction(axioms: List[Expression], assumptions: List[Expression], oldProof: Vector[Expression]): ArrayBuffer[Expression] = {
        val proof = ArrayBuffer[Expression]()
        val alpha = if (assumptions.last.getChild.length > 1) new Brackets(assumptions.last) else assumptions.last

        var i = 0
        for (a <- oldProof) {
            var proved = false
            if (Classic.checkAxioms(axioms, a) != -1 || Classic.checkAssumption(assumptions.init, a) != -1) {
//                proof += new Variable("Axiom or assumption")
                proved = true
                proof += a //writer.write(a + "\n")
                proof += new Implication(new Brackets(a), new Brackets(new Implication(alpha, a)))
            }

            if (!proved && a.equals(alpha)) {
//                proof += new Variable("Alpha")
                proved = true
                fromAtoA(proof, alpha)
            }

            val (x, y) = if (!proved) Classic.checkModusPonens(oldProof, i, a) else (-1, -1)
            if (x != -1) {
                val dj = oldProof(x)
                val dk = oldProof(y)
//                proof += new Variable("Modus: " + x + " - " + y)
                modus(proof, alpha, dj, dk, a)
            }

            //            proof += a
            proof += new Implication(alpha, a)
            i += 1
        }
        proof
    }

    def deduction(axioms: List[Expression], assumptions: List[Expression], oldProof: ArrayBuffer[Expression]): ArrayBuffer[Expression] = {
        deduction(axioms, assumptions, oldProof.toVector)
    }

    def fromAtoA(proof: ArrayBuffer[Expression], a: Expression) {
        proof += new Implication(a, new Brackets(new Implication(a, a)))
        proof += new Implication(
            new Brackets(new Implication(a, new Brackets(new Implication(a, a)))),
            new Implication(
                new Brackets(new Implication(a, new Brackets(new Implication(new Brackets(new Implication(a, a)), a)))),
                new Brackets(new Implication(a, a))
            )
        )
        proof += new Implication(
            new Brackets(new Implication(a, new Brackets(new Implication(new Brackets(new Implication(a, a)), a)))),
            new Brackets(new Implication(a, a))
        )
        proof += new Brackets(new Implication(a, new Brackets(new Implication(new Brackets(new Implication(a, a)), a))))
        //        writer.write(new Implication(a, a) + "\n")
    }

    def fromAtoA(writer: FileWriter, a: Expression) {
        writer.write(new Implication(a, new Brackets(new Implication(a, a))) + "\n")
        writer.write(new Implication(
            new Brackets(new Implication(a, new Brackets(new Implication(a, a)))),
            new Implication(
                new Brackets(new Implication(a, new Brackets(new Implication(new Brackets(new Implication(a, a)), a)))),
                new Brackets(new Implication(a, a))
            )
        ) + "\n")
        writer.write(new Implication(
            new Brackets(new Implication(a, new Brackets(new Implication(new Brackets(new Implication(a, a)), a)))),
            new Brackets(new Implication(a, a))
        ) + "\n")
        writer.write(new Brackets(new Implication(a, new Brackets(new Implication(new Brackets(new Implication(a, a)), a)))) + "\n")
//        writer.write(new Implication(a, a) + "\n")
    }

    def modus(writer: FileWriter, a: Expression, dj: Expression, dk:Expression, di: Expression): Unit = {
        writer.write(new Implication(
            new Brackets(new Implication(a, dj)),
            new Brackets(new Implication(new Brackets(new Implication(a, new Brackets(dk))), new Brackets(new Implication(a, di))))) + "\n")
        writer.write(new Brackets(new Implication(new Brackets(new Implication(a, new Brackets(dk))), new Brackets(new Implication(a, di)))) + "\n")
    }

    def modus(proof: ArrayBuffer[Expression], a: Expression, dj: Expression, dk:Expression, di: Expression): Unit = {
        proof += new Implication(
            new Brackets(new Implication(a, dj)),
            new Brackets(new Implication(new Brackets(new Implication(a, new Brackets(dk))), new Brackets(new Implication(a, di)))))
        proof += new Brackets(new Implication(new Brackets(new Implication(a, new Brackets(dk))), new Brackets(new Implication(a, di))))
    }
}
