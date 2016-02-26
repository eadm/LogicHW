package ru.nobird.scala.hw

import java.io.FileWriter

import ru.nobird.scala.Reader
import ru.nobird.scala.expression.formal.{UniversalQuantifier, ExistenceQuantifier}
import ru.nobird.scala.expression.{Conjunction, Implication, Brackets, Expression}
import ru.nobird.scala.logic.{Classic, Formal}
import ru.nobird.scala.parser.FormalMonadParser

import scala.collection.mutable.ArrayBuffer

/**
  * Created by ruslandavletshin on 20/02/16.
  */
object HW4 extends HW {
    def execute(reader: Reader, writer: FileWriter): (Boolean, String) = {
        val time = System.currentTimeMillis()

        val (beta :: assumptions) = FormalMonadParser.parseHeader(reader.readLine()).get

        val deduction = assumptions.nonEmpty

        var alpha: Expression = null

        if (deduction) {
            alpha = if (assumptions.last.getChild.length > 1) new Brackets(assumptions.last) else assumptions.last
            writer.write(assumptions.init.mkString(",") + "|-" + new Implication(alpha, beta) + "\n")
        } else {
            writer.write(s"|-$beta\n")
        }


        var i = 0
        var e = 0
        var last = time

        val proof = ArrayBuffer[Expression]()

        var wrong = false
        var error = ""
        var errorID = -1

        while (reader.ready() && !wrong) {
            val s = reader.readLine()
//            println(s)
            i += 1
            FormalMonadParser.parseLine(s) match {
                case Some(line) =>
                    var proved = false

                    if (Formal.checkAxioms(line) != -1 || // 1-8 aks
                        Formal.checkInduction(line) != -1 || // 9 - sc
                        Classic.checkAssumption(if(assumptions.nonEmpty) assumptions.init else List(), line) != -1 || // asm
                        Formal.checkSchemas(line) != -1) { // 1 - 10 schemas

                        proved = true
                        if (deduction) {
                            writer.write(line + "\n")
                            writer.write(new Implication(new Brackets(line), new Brackets(new Implication(alpha, line))) + "\n")
                        }
                    }

                    if (!proved) { // 11 scheme
                        Formal.check11Scheme(line) match {
                            case (_, _, true, msg) =>
                                proved = true
                                error = msg
                                wrong = true

                            case (true, v, _, _) =>
                                proved = true
//                                if (assumptions.last.getFreeVariables(Set()).contains(v)) {
//                                    error = s"используется схама аксиом с квантором по переменной $v, входящей свободно в допущение " + assumptions.last
//                                    wrong = true
//                                } else
                                if (deduction) {
                                    writer.write(line + "\n")
                                    writer.write(new Implication(new Brackets(line), new Brackets(new Implication(alpha, line))) + "\n")
                                }
                            case default =>
                        }
                    }

                    if (!proved) { // 12 scheme
                        Formal.check12Scheme(line) match {
                            case (_, _, true, msg) =>
                                proved = true
                                error = msg
                                wrong = true

                            case (true, v, _, _) =>
                                proved = true
//                                if (assumptions.last.getFreeVariables(Set()).contains(v)) {
//                                    error = s"используется схама аксиом с квантором по переменной $v, входящей свободно в допущение " + assumptions.last
//                                    wrong = true
//                                } else
                                if (deduction) {
                                    writer.write(line + "\n")
                                    writer.write(new Implication(new Brackets(line), new Brackets(new Implication(alpha, line))) + "\n")
                                }
                            case default =>
                        }
                    }

                    if (deduction && !proved && line.equals(alpha)) { // if aj == alpha
                        proved = true
                        HW2.fromAtoA(writer, alpha)
                    }

                    val (x, y) = if (!proved) Formal.checkModusPonens(proof, line) else (-1, -1) // mp
                    if (x != -1) {
                        //                        writer.write("Modus: " + x + " - " + y +"\n")
                        proved = true
                        val dj = proof(x)
                        val dk = proof(y)
                        if (deduction)
                           HW2.modus(writer, alpha, dj, dk, line)
                    }

                    if (!proved) { // universal quantifier rule
                        Formal.checkUniversalQuantifierRule(proof, line) match {
                            case (_, _, true, msg, _) =>
                                proved = true
                                error = msg
                                wrong = true

                            case (true, v, _, _, _) =>
                                proved = true
                                if (deduction) {
                                    if (alpha.getFreeVariables(Set()).contains(v)) {
                                        error = s"используется правило с квантором по переменной $v, входящей свободно в допущение " + alpha
                                        wrong = true
                                    } else
                                        universalQuantifierRule(writer, line, alpha)
                                }
                            case default =>
                        }
                    }

                    if (!proved) { // existence quantifier rule
                        Formal.checkExistenceQuantifierRule(proof, line) match {
                            case (_, _, true, msg, _) =>
                                proved = true
                                error = msg
                                wrong = true

                            case (true, v, _, _, _) =>
                                proved = true
                                if (deduction) {
                                    if (alpha.getFreeVariables(Set()).contains(v)) {
                                        error = s"используется правило с квантором по переменной $v, входящей свободно в допущение " + alpha
                                        wrong = true
                                    } else
                                        existenceQuantifierRule(writer, line, alpha)
                                }
                            case default =>
                        }
                    }

                    if (deduction) {
                        writer.write(new Implication(alpha, line).toString + "\n")
                    } else {
                        writer.write(line + "\n")
                    }

                    if (!proved) wrong = true
                    if (wrong) {
//                        println(line)
                        errorID = i
                    }

                    proof += line
                case None =>
                    e += 1
                    println("Error: " + s)
            }



            if (i % 100 == 0) {
                println("Position: " + i + "   Total: " + (System.currentTimeMillis() - time) + "ms" + "   Threshold: " + ((System.currentTimeMillis() - last) / 100.0) + " ms/line")
                last = System.currentTimeMillis()
            }

        }


        println("Total: " + i + " with " + e + " errors")
        if (wrong)
            (false, s"Вывод некорректен начиная с формулы номер $errorID" + (if (error.length > 0) s": $error" else ""))
        else
            (true, "")
    }

    def universalQuantifierRule(writer: FileWriter, line: Expression, a: Expression) = { // psi -> @x fi
        val target = line.reduceBrackets
        val v = target.getChild.last.reduceBrackets.getLinkedVariables.head
        val fi = new Brackets(target.getChild.last.reduceBrackets.getChild.head) // fi
        val psi = new Brackets(target.getChild.head) // psi
        val alpha = new Brackets(a)

        for( l <- lemma2(alpha, psi, fi)) writer.write(l + "\n")

        writer.write(new Implication(new Conjunction(alpha, psi), fi) + "\n")
        writer.write(new Implication(new Conjunction(alpha, psi), new UniversalQuantifier(v, fi)) + "\n")

        for( l <- lemma1(alpha, psi, new UniversalQuantifier(v, fi))) writer.write(l + "\n")
    }

    def lemma1(a: Expression, b: Expression, c: Expression) = {
        deductionToTheEnd(
            List(
                new Implication(new Brackets(new Conjunction(a, b)), c),
                a,
                b
            ),
            ArrayBuffer[Expression](
                a,
                b,
                new Implication(a, new Implication(b, new Conjunction(a, b))),
                new Implication(b, new Conjunction(a, b)),
                new Conjunction(a, b),
                new Implication(new Brackets(new Conjunction(a, b)), c),
                c
            )
        )
    }

    def lemma2(a: Expression, b: Expression, c:Expression) = {
        deductionToTheEnd(
            List(
                new Implication(a, new Implication(b, c)),
                new Conjunction(a, b)
            ),
            ArrayBuffer[Expression](
                new Implication(new Conjunction(a, b), a),
                new Conjunction(a, b),
                a,
                new Implication(new Conjunction(a, b), b),
                b,
                new Implication(a, new Implication(b, c)),
                new Implication(b, c),
                c
            )
        )
    }

    def existenceQuantifierRule(writer: FileWriter, line: Expression, a: Expression) = {
        val target = line.reduceBrackets
        val v = target.getChild.head.reduceBrackets.getLinkedVariables.head
        val psi = new Brackets(target.getChild.head.reduceBrackets.getChild.head) // psi
        val fi = new Brackets(target.getChild.last) // fi
        val alpha = new Brackets(a)


        for( l <- permutation(alpha, psi, fi)) writer.write(l + "\n")

        writer.write(new Implication(alpha, new Brackets(new Implication(psi, fi))) + "\n")
        writer.write(new Implication(psi, new Brackets(new Implication(alpha, fi))) + "\n")

        writer.write(new Implication(new ExistenceQuantifier(v, psi), new Brackets(new Implication(alpha, fi))) + "\n")

        for( l <- permutation(new ExistenceQuantifier(v, psi), alpha, fi)) writer.write(l + "\n")
        // последняя строчка сама пишется в коде выше
    }

    def permutation(alpha: Expression, psi: Expression, fi: Expression): ArrayBuffer[Expression] = {
        deductionToTheEnd(
            List(
                new Implication(alpha, new Brackets(new Implication(psi, fi))),
                psi,
                alpha
            ),
            ArrayBuffer[Expression](
                new Implication(alpha, new Brackets(new Implication(psi, fi))),
                alpha,
                new Implication(psi, fi),
                psi,
                fi
            )
        )
    }

    def deductionToTheEnd(assumptions: List[Expression], proof: ArrayBuffer[Expression]): ArrayBuffer[Expression] = {
        assumptions match {
            case List(l) => HW2.deduction(Classic.getAxioms, assumptions, proof)
            case default => deductionToTheEnd(assumptions.init, HW2.deduction(Classic.getAxioms, assumptions, proof))
        }
    }
}
