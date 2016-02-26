package ru.nobird.scala.hw

import java.io.FileWriter

import ru.nobird.scala.Reader
import ru.nobird.scala.expression.{Implication, Expression}
import ru.nobird.scala.expression.formal.{Equals ,Suc, Zero}
import ru.nobird.scala.parser.{ClassicMonadParser, FormalMonadParser}

/**
  * Created by ruslandavletshin on 25/02/16.
  */
object HW5 extends HW {

    def truth_s = "(A->B->A)"
    def truth = FormalMonadParser.parseLine(truth_s).get

    def execute(reader: Reader, writer: FileWriter): (Boolean, String) = {

        val (a, b) = (reader.nextInt(), reader.nextInt())

        writer.write("|-"+intToFormalS(a) + "+" + intToFormalS(b) + "=" + intToFormalS(a + b) + "\n")

//        println(intToFormalS(0))


        writer.write(truth + "\n")

        for (l <- base(intToFormalS(a))) writer.write(l + "\n")
        if (b > 0)
            for (l <- aEqualsA()) writer.write(l + "\n")

        for (k <- 0 until b) {
            for (l <- fromKtoK1(intToFormalS(a), intToFormalS(k), intToFormalS(a + k))) writer.write(l + "\n")
        }
        (true, "")
    }

    def base(a: String) = { // a+0=a
        List(
            "a+0=a",
            s"(a+0=a)->$truth_s->(a+0=a)",
            s"$truth_s->(a+0=a)",
            s"$truth_s->@a(a+0=a)",
            "@a(a+0=a)",
            s"@a(a+0=a)->($a+0=$a)",
            s"$a+0=$a"
        )
    }

    def fromKtoK1(a: String, k: String, sum: String) = {
        List( // (a + k)' = sum'
            "a=b->a'=b'",
            s"(a=b->a'=b')->$truth_s->(a=b->a'=b')",
            s"$truth_s->(a=b->a'=b')",
            s"$truth_s->@b(a=b->a'=b')",
            s"$truth_s->@a@b(a=b->a'=b')",
            "@a@b(a=b->a'=b')",
            s"@a@b(a=b->a'=b')->@b($a+$k=b->($a+$k)'=b')",
            s"@b($a+$k=b->($a+$k)'=b')",
            s"@b($a+$k=b->($a+$k)'=b')->($a+$k=$sum->($a+$k)'=$sum')",
            s"($a+$k=$sum->($a+$k)'=$sum')",
            s"($a+$k)'=$sum'"
        ) ++ List( // a + k' = (a + k)'
            "a+b'=(a+b)'",
            s"(a+b'=(a+b)')->$truth_s->(a+b'=(a+b)')",
            s"$truth_s->(a+b'=(a+b)')",
            s"$truth_s->@b(a+b'=(a+b)')",
            s"$truth_s->@a@b(a+b'=(a+b)')",
            "@a@b(a+b'=(a+b)')",
            s"@a@b(a+b'=(a+b)')->@b($a+b'=($a+b)')",
            s"@b($a+b'=($a+b)')",
            s"@b($a+b'=($a+b)')->($a+$k'=($a+$k)')",
            s"$a+$k'=($a+$k)'"
        ) ++ List( // (a + k)' = a + k' -> (a + k)' = sum' -> a + k' = sum'
            "a=b->a=c->b=c",
            s"(a=b->a=c->b=c)->$truth_s->(a=b->a=c->b=c)",
            s"$truth_s->(a=b->a=c->b=c)",
            s"$truth_s->@c(a=b->a=c->b=c)",
            s"$truth_s->@b@c(a=b->a=c->b=c)",
            s"$truth_s->@a@b@c(a=b->a=c->b=c)",
            "@a@b@c(a=b->a=c->b=c)",
            s"@a@b@c(a=b->a=c->b=c)->@b@c(($a+$k)'=b->($a+$k)'=c->b=c)",
            s"@b@c(($a+$k)'=b->($a+$k)'=c->b=c)",
            s"@b@c(($a+$k)'=b->($a+$k)'=c->b=c)->@c(($a+$k)'=($a+$k')->($a+$k)'=c->($a+$k')=c)",
            s"@c(($a+$k)'=($a+$k')->($a+$k)'=c->($a+$k')=c)",
            s"@c(($a+$k)'=($a+$k')->($a+$k)'=c->($a+$k')=c)->(($a+$k)'=($a+$k')->($a+$k)'=$sum'->($a+$k')=$sum')",
            s"(($a+$k)'=($a+$k')->($a+$k)'=$sum'->($a+$k')=$sum')"
        ) ++ List( // тут доказываем (a + k)' = a + k'
            "a=b->a=c->b=c",
            s"(a=b->a=c->b=c)->$truth_s->(a=b->a=c->b=c)",
            s"$truth_s->(a=b->a=c->b=c)",
            s"$truth_s->@c(a=b->a=c->b=c)",
            s"$truth_s->@b@c(a=b->a=c->b=c)",
            s"$truth_s->@a@b@c(a=b->a=c->b=c)",
            "@a@b@c(a=b->a=c->b=c)",
            s"@a@b@c(a=b->a=c->b=c)->@b@c(($a+$k')=b->($a+$k')=c->b=c)",
            s"@b@c(($a+$k')=b->($a+$k')=c->b=c)",
            s"@b@c(($a+$k')=b->($a+$k')=c->b=c)->@c(($a+$k')=($a+$k)'->($a+$k')=c->($a+$k)'=c)",
            s"@c(($a+$k')=($a+$k)'->($a+$k')=c->($a+$k)'=c)",
            s"@c(($a+$k')=($a+$k)'->($a+$k')=c->($a+$k)'=c)->(($a+$k')=($a+$k)'->($a+$k')=$a+$k'->($a+$k)'=$a+$k')",
            s"(($a+$k')=($a+$k)'->($a+$k')=$a+$k'->($a+$k)'=$a+$k')"
        ) ++ List(
            s"($a+$k'=$a+$k')->(($a+$k)'=$a+$k')"
        ) ++ List( // a + k' = a + k'
            s"(a=a)->$truth_s->(a=a)",
            s"$truth_s->(a=a)",
            s"$truth_s->@a(a=a)",
            s"@a(a=a)",
            s"@a(a=a)->($a+$k'=$a+$k')",
            s"($a+$k'=$a+$k')"
        ) ++ List( // sum' = a + k'
            s"(($a+$k)'=$a+$k')"
        ) ++ List( // 2 mp => a + k' = sum'
            s"($a+$k)'=$sum'->($a+$k')=$sum'",
            s"$a+$k'=$sum'"
        )
    }

    def termEqualsTerm(term: Expression) =
        ClassicMonadParser.parseLines(
            List(
                s"a=a->$truth_s->a=a",
                s"$truth_s->a=a",
                s"$truth_s->@a(a=a)",
                "@a(a=a)"
            ),
            FormalMonadParser.parseLine
        ) ++ List(
            new Implication(FormalMonadParser.parseLine("@a(a=a)").get, new Equals(term, term)), //"@a(a=a)->(x=x)"
            new Equals(term, term)
        )


    def aEqualsA() = {

        List(
            "a=b->a=c->b=c",
            s"(a=b->a=c->b=c)->$truth_s->(a=b->a=c->b=c)",
            s"$truth_s->(a=b->a=c->b=c)",
            s"$truth_s->@c(a=b->a=c->b=c)",
            s"$truth_s->@b@c(a=b->a=c->b=c)",
            s"$truth_s->@a@b@c(a=b->a=c->b=c)",
            "@a@b@c(a=b->a=c->b=c)",
            "@a@b@c(a=b->a=c->b=c)->@b@c(a+0=b->a+0=c->b=c)",
            "@b@c(a+0=b->a+0=c->b=c)",
            "@b@c(a+0=b->a+0=c->b=c)->@c(a+0=a->a+0=c->a=c)",
            "@c(a+0=a->a+0=c->a=c)",
            "@c(a+0=a->a+0=c->a=c)->(a+0=a->a+0=a->a=a)",
            "(a+0=a->a+0=a->a=a)",
            "a+0=a",
            "a+0=a->a=a",
            "a=a"
        )
    }

    def intToFormalS(k: Int): String = {
        val sb = new StringBuilder(k + 1)
        sb += '0'
        for (_ <- 0 until k) sb += '''
        sb.toString()
    }
}
