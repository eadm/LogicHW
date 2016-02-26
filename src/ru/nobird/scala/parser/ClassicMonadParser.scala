package ru.nobird.scala.parser

import ru.nobird.scala.expression._


/**
  * Created by ruslandavletshin on 23/11/15.
  */
object ClassicMonadParser {
    def createHeaderParser():Parser[List[Expression]] = {
        for {
            asx <- plus[List[Expression]]( // assumptions
                for {
                    a <- implication()
                    ax <- many(for (_ <- sym(','); x <- implication()) yield x)
                } yield a :: ax,
                success(List[Expression]())
            )
            _ <- str("|-")
            t <- implication()
        } yield t :: asx // head element is a target, rest is an assumptions
    }

    def createExpressionParser() = implication()

    def parseLine(s: String): Option[Expression] = {
        createExpressionParser()(s) match {
            case x :: _ =>
                new Some[Expression](x._1)
            case default => None
        }
    }

    def parseLines(lines: Seq[String], parser: String => Option[Expression]) = for (line <- lines) yield parser(line).get
    def insert(lines: Seq[Expression], values: Map[String, Expression]) = for (line <- lines) yield line.insertExpression(values)

    def right(next: Parser[Expression], operator: String, factory: ((Expression, Expression) => Expression)): Parser[Expression] = {
        for {
            l <- next
            rr <- plus[Expression](
                for {
                    _ <- str(operator)
                    r <- right(next, operator, factory)
                } yield r,
                success(new None)
            )
        } yield {
            if (rr.getType == ExpressionType.None)
                l
            else
                factory(l, rr)
        }
    }

    def left(
                 next: Parser[Expression],
                 operator: String,
                 factory: (Expression, Expression) => Expression,
                 p: Expression
             ): Parser[Expression] = {
        for {
            l <- next
            rr <- plus[Expression](
                for {
                    _ <- str(operator)
                    r <- left(next, operator, factory, if (p!= null) factory(p,l) else l)//(e1, e2) => factory(factory(l, e1), e2))
                } yield r,
                success(new None)
            )
        } yield {
            if (rr.getType == ExpressionType.None)
                if (p != null) factory(p, l) else l
            else
                rr
        }
    }



    def implication() = right(disjunction(), "->", (a, b) => new Implication(a, b))
    def disjunction() = left(conjunction(), "|", (a, b) => new Disjunction(a, b), null)
    def conjunction() = left(negate(), "&", (a, b) => new Conjunction(a, b), null)

    def negate(): Parser[Expression] = {
        for {
            x <- plus[Expression](
                for {
                    _ <- sym('!')
                    v <- negate()
                } yield new Negation(v),
                for (
                    v <- brackets()
                ) yield v
            )
        } yield x // add here some negation
    }

    def brackets(): Parser[Expression] = {
        for (
            x <- plus[Expression](
                for {
                    _ <- sym('(')
                    v <- implication()
                    _ <- sym(')')
                } yield new Brackets(v),
                for (
                    v <- variable()
                ) yield v
            )
        ) yield x
    }

    def variable(): Parser[Expression] = {
        for (symbols <- many1(isLetterOrDigit)) yield new Variable(symbols.mkString)
    }

    def item(): Parser[Char] = {
        new Parser[Char]({s: String => {
            if (s.isEmpty) Nil
            else List((s.head, s.tail))
        }})
    }

    def sat(predicate: Char => Boolean): Parser[Char] =
    item().flatMap(c => if (predicate(c)) success(c) else failure(c))

    def sym(c: Char) = sat(_ == c)
    def isLetterOrDigit = sat(_.isLetterOrDigit)
    def isBigLetterOrDigit = sat((x: Char) => x.isUpper || x.isDigit)
    def isLowLetterOrDigit = sat((x: Char) => x.isLower || x.isDigit)

    def str(s: String): Parser[String] = // checking for a string
        if (s.isEmpty)
            success("")
        else
            for (_ <- sym(s.head); _ <- str(s.tail)) yield s


//    def plus[A](p1: Parser[A], p2: Parser[A]): Parser[A] = new Parser[A]({s: String =>
//        val pr1 = p1(s)
//        if (pr1.nonEmpty) List(pr1.head) else List(p2(s).head)
//    })
//    def plus[A](p1: Parser[A], p2: Parser[A]): Parser[A] = new Parser[A]({s: String => p1(s) ++ p2(s)})
    def plus[A](p1: Parser[A], p2: Parser[A]): Parser[A] = new Parser[A]({
        s: String => (p1(s), p2(s)) match {
            case (p11 :: _, p22 :: _) => List(p11, p22)
            case (p11 :: _, List()) => List(p11)
            case (List(), p22 :: _) => List(p22)
            case default => List()
        }

    })

    def plusMany[A](ps: List[Parser[A]]): Parser[A] = ps match {
        case List(p) => p
        case p :: px => plus[A](p, plusMany(px))
    }

    def many[A](parser: Parser[A]): Parser[List[A]] =
        plus(
            for (a <- parser; as <- many(parser)) yield a::as,
            success(List[A]())
        )


    def many1[A](parser: Parser[A]): Parser[List[A]] = for (a <- parser; as <- many(parser)) yield a::as


    def success[A](a: A) = { // unit
        new Parser[A]({s: String => List((a, s))})
    }

    def failure[A](a: A) = {
        new Parser[A]({_: String => Nil})
    }
}
