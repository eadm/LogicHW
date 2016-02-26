package ru.nobird.scala.logic

import ru.nobird.scala.{Util, Reader}
import ru.nobird.scala.expression.{ExpressionType, Expression}
import ru.nobird.scala.parser.ClassicMonadParser

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Created by ruslandavletshin on 08/02/16.
  */
object Classic {
    def checkAssumption(assumptions: List[Expression], e: Expression) = assumptions.indexWhere(x => x.equals(e))

    def checkModusPonens(proof: ArrayBuffer[Expression], target: Expression): (Int, Int) = {
        var b = 0
        val a = proof.lastIndexWhere(p1 => {
            val nb = Util.escapeBrackets(p1)
            if (nb.getType == ExpressionType.Implication && nb.getChild.last.equals(target)) {
                b = proof.lastIndexWhere(p2 => nb.getChild.head.equals(p2))
                b != -1
            } else false
        })

        if (a != -1) {
            (b, a)
        } else (-1, -1)
    }

    def checkModusPonens(proof: Vector[Expression], end: Int, target: Expression): (Int, Int) = {
        var b = 0
        val a = proof.lastIndexWhere(p1 => {
            val nb = Util.escapeBrackets(p1)
            if (nb.getType == ExpressionType.Implication && nb.getChild.last.equals(target)) {
                b = proof.lastIndexWhere(p2 => nb.getChild.head.equals(p2), end)
                b != -1
            } else false
        }, end)

        if (a != -1) {
            (b, a)
        } else (-1, -1)
    }

    val getAxioms = schemas()

    def schemas():List[Expression] = {
        val reader = new Reader("assets/classic/schemas")
        val listBuf = new ListBuffer[Expression]()

        while (reader.ready()) listBuf.append(ClassicMonadParser.createExpressionParser()(reader.readLine()).head._1)
        reader.close()
        listBuf.toList
    }

    def checkAxioms(axioms: List[Expression], target: Expression): Int = axioms.indexWhere(p => checkAxiom(p, target, Map[String, Expression]())._1)

    private def recursion(axs: List[Expression], txs: List[Expression], variables: Map[String, Expression]): (Boolean, Map[String, Expression]) = {
        if (axs.isEmpty) (true, variables) else {
            val aaa = checkAxiom(axs.head, txs.head, variables) // отправляем на проверку 1-го ребенка аксиомы и выражения
            val bbb = recursion(axs.tail, txs.tail, variables) // отправляем на проверку остальных ... тупо цикл

            if (!aaa._1 || !bbb._1) (false, variables) else { // если что то пошло не так, то нам это уже не интересно
                (!aaa._2.exists(p1 => bbb._2.exists(p2 => p1._1 == p2._1 && !p1._2.equals(p2._2))), aaa._2 ++ bbb._2) // проверка, есть ли две переменные с разными оценками
            }
        }
    }

    def checkAxiom(axiom: Expression, target: Expression, variables: Map[String, Expression]): (Boolean, Map[String, Expression]) = { // проверка - индукция по структуре ф-лы
        if (axiom.getType != ExpressionType.Variable) { // если аксиома не переменная
            if (axiom.getType == ExpressionType.Brackets && target.getType != ExpressionType.Brackets) {
                checkAxiom(axiom.getChild.head, target, variables)
            } else
            if (axiom.getType != ExpressionType.Brackets && target.getType == ExpressionType.Brackets) {
                checkAxiom(axiom, target.getChild.head, variables)
            } else
            if (axiom.getType != target.getType) { // если типы аксиомы и выражения не совпадают, то нам не попути
                (false, variables)
            } else {
                recursion(axiom.getChild, target.getChild, variables) // отправляемся проверять детей аксиомы и выражения
            }
        } else {
            variables.get(axiom.toString) match { // если аксиома - переменная
                case Some(x: Expression) => if (x.equals(target)) (true, variables) else (false, variables) // проверяем совпадает ли оценка, которую мы хотим дать с текущей
                case None => (true, Map(axiom.toString -> target) ++ variables) // задаем оценку новой переменной
            }
        }
    }
}
