package ru.nobird.scala.hw

import java.io.FileWriter

import ru.nobird.scala.Reader
import ru.nobird.scala.expression.Expression
import ru.nobird.scala.logic.Classic
import ru.nobird.scala.parser.ClassicMonadParser

import scala.collection.mutable.ArrayBuffer

/**
  * Created by ruslandavletshin on 07/02/16.
  */
object HW1 extends HW {
    def main(args: Array[String]): Unit = {
        val file = "test"

        val reader = new Reader(file + ".in")
        val writer = new FileWriter(file + ".out")

        execute(reader, writer)

        writer.close()
        reader.close()
    }


    def execute(reader: Reader, writer: FileWriter): (Boolean, String) = {
        val time = System.currentTimeMillis()



        val headerParser = ClassicMonadParser.createHeaderParser()
        val parser = ClassicMonadParser.createExpressionParser()
        val axioms = Classic.schemas()

        val (target :: assumptions) = headerParser(reader.readLine()).head._1

        val proof = ArrayBuffer[Expression]()

        var last = time

        var i = 0
        while (reader.ready()) {
            val s = reader.readLine()
            i += 1
            parser(s) match {
                case a :: as =>
                    var proved = false

                    writer.write("(" + i + ") " + a._1 + " (")

                    val asmp = Classic.checkAssumption(assumptions, a._1) + 1 // cause everything from 0, but we from 1
                    if (asmp != 0) {
                        proved = true
                        writer.write("Предп. " + asmp)
                    } // check if line is assumption

                    val ax = if(!proved) Classic.checkAxioms(axioms, a._1) else -1
                    if (ax != -1) {
                        proved = true
                        writer.write("Сх. акс. " + (ax + 1))
                    } // check for axioms

                    val (x, y) = if (!proved) Classic.checkModusPonens(proof, a._1) else (-1, -1)
                    if (x != -1) {
                        proved = true
                        writer.write("M.P. " + (x + 1) + ", " + (y + 1))
                    } // check for mp

                    if (!proved) {
                        writer.write("Не доказано")
                        println("Err "+ i +": " + a._1)
                    }

                    if (i % 100 == 0) {
                        println("Position: " + i + "   Total: " + (System.currentTimeMillis() - time) + "ms" + "   Threshold: " + ((System.currentTimeMillis() - last) / 100.0) + " ms/line")
                        last = System.currentTimeMillis()
                    }

                    proof += a._1
                    writer.write(")\n")

                case default => println("Error: " + s)
            }
        }
        (true, "")
    }
}
