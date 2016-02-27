package ru.nobird.scala.parser

import ru.nobird.scala.expression.formal._
import ru.nobird.scala.expression._


/**
  * Created by ruslandavletshin on 20/02/16.
  */
object FormalMonadParser {
    def parseLine(s: String): Option[Expression] = {
        getFormalExpressionParser(s) match {
            case x :: _ => new Some[Expression](x._1)
            case default => None
        }
    }

    def parseHeader(s: String): Option[List[Expression]] = {
        getFormalHeaderParser(s)  match {
            case x :: _ => new Some(x._1)
            case default => None
        }
    }


    val getFormalHeaderParser = header()

    def header() = {
        for {
            asx <- ClassicMonadParser.plus[List[Expression]]( // assumptions
                for {
                    a <- expression()
                    ax <- ClassicMonadParser.many(for (_ <- ClassicMonadParser.sym(','); x <- expression()) yield x)
                } yield a :: ax,
                ClassicMonadParser.success(List[Expression]())
            )
            _ <- ClassicMonadParser.str("|-")
            t <- expression()
        } yield t :: asx
    }

    val getFormalExpressionParser = expression()
    def expression(): Parser[Expression] = implication()

    def implication() = ClassicMonadParser.right(disjunction(), "->", (a, b) => new Implication(a, b))
    def disjunction() = ClassicMonadParser.left(conjunction(), "|", (a, b) => new Disjunction(a, b), null)
    def conjunction() = ClassicMonadParser.left(unary(), "&", (a, b) => new Conjunction(a, b), null)

    def unary(): Parser[Expression] = {
        for {
            p <- ClassicMonadParser.plusMany[Expression](List[Parser[Expression]](
                predicate(),
                for {
                    _       <- ClassicMonadParser.sym('!')
                    v       <- unary()
                } yield new Negation(v),
                for {
                    _       <- ClassicMonadParser.sym('(')
                    b       <- expression()
                    _       <- ClassicMonadParser.sym(')')
                } yield new Brackets(b),
                for {
                    _       <- ClassicMonadParser.sym('@') // universal
                    v       <- variable()
                    u       <- unary()
                } yield new UniversalQuantifier(v.toString, u),
                for {
                    _       <- ClassicMonadParser.sym('?') // existence
                    v       <- variable()
                    u       <- unary()
                } yield new ExistenceQuantifier(v.toString, u)
            ))
        } yield p
    }

    def predicate(): Parser[Expression] = {
        for {
            p <- ClassicMonadParser.plusMany[Expression](List[Parser[Expression]](
                for {// func
                    name    <- ClassicMonadParser.many1(ClassicMonadParser.isBigLetterOrDigit)
                    _       <- ClassicMonadParser.sym('(')
                    arg     <- term()
                    args    <- ClassicMonadParser.many(for (_ <- ClassicMonadParser.sym(','); ax <- term()) yield ax)
                    _       <- ClassicMonadParser.sym(')')
                } yield new Predicate(name.mkString, arg :: args),
                for {
                    a       <- term()
                    _       <- ClassicMonadParser.sym('=')
                    b       <- term()
                } yield new Equals(a, b),
                for {
                    name    <- ClassicMonadParser.many1(ClassicMonadParser.isBigLetterOrDigit)
                } yield new Predicate(name.mkString, List())
            ))
        } yield p
    }

    def term() = add()

    def add() = ClassicMonadParser.left(mul(), "+", (a, b) => new Add(a, b), null)
    def mul() = ClassicMonadParser.left(suc(), "*", (a, b) => new Mul(a, b), null)

    def suc(): Parser[Expression] = {
        for {
            a <- ClassicMonadParser.plus[Expression](
                for {
                    b       <- func()
                    ss      <- ClassicMonadParser.many(for {_ <- ClassicMonadParser.str("'")} yield 0)
                } yield (b /: ss)((b, _) => new Suc(b)), // fold left
                func()
            )
        } yield a
    }

    def func(): Parser[Expression] = {
        for (
            p <- ClassicMonadParser.plusMany[Expression](List[Parser[Expression]](
                for {// func
                    name    <- ClassicMonadParser.many1(ClassicMonadParser.isLowLetterOrDigit)
                    _       <- ClassicMonadParser.sym('(')
                    arg     <- term()
                    args    <- ClassicMonadParser.many(for (_ <- ClassicMonadParser.sym(','); ax <- term()) yield ax)
                    _       <- ClassicMonadParser.sym(')')
                } yield new Func(name.mkString, arg :: args),
                for {
                    _       <- ClassicMonadParser.sym('0')
                } yield new Zero(),
                for {
                _       <- ClassicMonadParser.sym('(')
                b       <- term()
                _       <- ClassicMonadParser.sym(')')
                } yield b,
                variable()
            ))
        ) yield p
    }


    def variable(): Parser[Expression] = {
        for (symbols <- ClassicMonadParser.many1(ClassicMonadParser.isLowLetterOrDigit)) yield new Variable(symbols.mkString)
    }

}
