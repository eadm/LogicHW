package ru.nobird.scala.logic

import ru.nobird.scala.expression.formal.{Suc, UniversalQuantifier, Zero}
import ru.nobird.scala.expression._
import ru.nobird.scala.parser.{ClassicMonadParser, FormalMonadParser}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by ruslandavletshin on 22/02/16.
  */
object Formal {

    def checkModusPonens(proof: ArrayBuffer[Expression], target: Expression) = Classic.checkModusPonens(proof, target)

    def axioms =
        for (line <- Source.fromFile("assets/formal/axioms").getLines()) yield FormalMonadParser.parseLine(line).get

    def schemas =
        for (line <- Source.fromFile("assets/formal/schemas").getLines()) yield ClassicMonadParser.parseLine(line).get

    def checkSchemas(target: Expression): Int = schemas.indexWhere(p => Classic.checkAxiom(p, target, Map[String, Expression]())._1)
    def check11Scheme(t: Expression): (Boolean, String, Boolean, String) = {
        val target = t.reduceBrackets
        if (target.getType == ExpressionType.Implication) {
            val u = target.getChild.head.reduceBrackets // any x phi
            val e = target.getChild.last.reduceBrackets // phi

//            val (m, map) = Classic.checkAxiom(u.getChild.head, e, Map[String, Expression]())
//            val (m, map) =
//                if (u.getChild.nonEmpty)
//                    Classic.checkAxiom(u.getChild.head, e, Map[String, Expression]())
//                else (false, null)

            if (u.getType == ExpressionType.UniversalQuantifier) {
                val (m, map) = Classic.checkAxiom(u.getChild.head, e, Map[String, Expression]())
                if (m)
                    map.get(u.getLinkedVariables.head) match {
                        case Some (theta) =>
                            isAvailableToInsert(u.getChild.head, u.getLinkedVariables.head, theta) // можно ли вставить вместо x в phi theta
                        case default => (true, u.getLinkedVariables.head, false, "") // если квантор был по переменной, которая нигде не использовалась
                    }
                else (false, "", false, "")
            } else
                (false, "", false, "")
        } else (false, "", false, "")
    }

    def check12Scheme(t: Expression): (Boolean, String, Boolean, String) = {
        val target = t.reduceBrackets
        if (target.getType == ExpressionType.Implication) {
            val e = target.getChild.head.reduceBrackets // phi
            val u = target.getChild.last.reduceBrackets // exists phi

            if (u.reduceBrackets.getType == ExpressionType.ExistenceQuantifier) {
                val (m, map) = Classic.checkAxiom(u.getChild.head, e, Map[String, Expression]())
                if (m)
                    map.get(u.getLinkedVariables.head) match {
                        case Some (theta) =>
                            isAvailableToInsert(u.getChild.head, u.getLinkedVariables.head, theta) // можно ли вставить вместо x в phi theta
                        case default => (true, u.getLinkedVariables.head, false, "") // если квантор был по переменной, которая нигде не использовалась
                    }
                else (false, "", false, "")
            } else
                (false, "", false, "")
        } else (false, "", false, "")
    }

    private def isAvailableToInsert(target: Expression, v: String, theta: Expression): (Boolean, String, Boolean, String) = { // проверяет, можно ли заменить в target переменную v на терм theta
        val linked = target.getLinkedVariablesOnWayToX(v, Set())
        val freed = theta.getFreeVariables(Set())
        val intersection = linked.intersect(freed)
        if(intersection.isEmpty || intersection == Set(theta.toString)) {// тут или тк могут быть случаи, когда мы хотим заменить x на x
            (true, v, false, "")
        } else (false, v, true, s"терм $theta не свооден для подстановки в формулу $target вместо переменной $v")
    }


    def checkUniversalQuantifierRule(proof: Seq[Expression], t: Expression): (Boolean, String, Boolean, String, Int) = {
        val target = t.reduceBrackets
        if (target.getType == ExpressionType.Implication) {

            val e = target.getChild.head.reduceBrackets //fi
            val u = target.getChild.last.reduceBrackets //@x psi

            if (u.getType == ExpressionType.UniversalQuantifier) {
                val v = u.getLinkedVariables.head // x

                if (e.getFreeVariables(Set()).contains(v)) { // если x входит свободно в fi то это не ок как то
                    (false, "", true, s"переменная $v входит свободно в формулу $e", -1)
                } else {
                    val pattern = new Implication(e, u.getChild.head)
                    val index = proof.lastIndexWhere(p => p.equals(pattern))
                    (index != -1, v, false, "", index)
                }

            } else (false, "", false, "", -1)
        } else (false, "", false, "", -1) // (isRule, isError, errorMsg)
    }

    def checkExistenceQuantifierRule(proof: Seq[Expression], t: Expression): (Boolean, String, Boolean, String, Int) = {
        val target = t.reduceBrackets
        if (target.getType == ExpressionType.Implication) {

            val u = target.getChild.head.reduceBrackets //?x psi
            val e = target.getChild.last.reduceBrackets //fi

            if (u.getType == ExpressionType.ExistenceQuantifier) {
                val v = u.getLinkedVariables.head // x

                if (e.getFreeVariables(Set()).contains(v)) { // если x входит свободно в fi то это не ок как то
                    (false, "", true, s"переменная $v входит свободно в формулу $e", -1)
                } else {
                    val pattern = new Implication(u.getChild.head, e)
                    val index = proof.lastIndexWhere(p => p.equals(pattern))
                    (index != -1, v, false, "", index)
                }

            } else (false, "", false, "", -1)
        } else (false, "", false, "", -1) // (isRule, isError, errorMsg)
    }

    def checkAxioms(target: Expression): Int = axioms.indexWhere(p => Classic.checkAxiom(p, target, Map[String, Expression]())._1)
    def checkInduction(t: Expression): Int = {
        val target = t.reduceBrackets
        if (target.getType != ExpressionType.Implication) -1
        else {
            val fi = target.getChild.last
            if ((false /: fi.getFreeVariables(Set()))((b, v) => {
                val ax = new Implication(new Conjunction(
                    fi.insertExpression(Map(v -> new Zero())),
                    new UniversalQuantifier(v, new Implication(fi, fi.insertExpression(Map(v -> new Suc(new Variable(v))))))
                ), fi)
                b || target.equals(ax)
            })) 9 else -1
        }
    }


}
