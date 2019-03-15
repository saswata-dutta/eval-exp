package org.saswata.expressions

import org.saswata.expressions.Expression.Exp
import org.saswata.expressions.Utils._

object Expression {

  sealed trait Exp[R] {
    def eval(env: Map[String, Any]): R
  }

  case class BOOL_SYMBOL(key: String) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean =
      env.get(key).collect {
        case b: Boolean => b
        case s: String => s.toBoolean
      }.getOrElse(false)
  }

  case class STR_SYMBOL(key: String) extends Exp[String] {
    override def eval(env: Map[String, Any]): String = env.get(key).map(_.toString).getOrElse("")
  }

  case class STR_LITERAL(value: String) extends Exp[String] {
    override def eval(env: Map[String, Any]): String = value
  }

  case class NUM_SYMBOL(key: String) extends Exp[Double] {
    override def eval(env: Map[String, Any]): Double = env.get(key).collect {
      case n: java.lang.Number => n.doubleValue()
      case s: String => s.toDouble
    }.getOrElse(0.0)
  }

  case class NUM_LITERAL(value: Double) extends Exp[Double] {
    override def eval(env: Map[String, Any]): Double = value
  }

  case class STR_EQUALS(lhs: Exp[String], rhs: Exp[String]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = lhs.eval(env) == rhs.eval(env)
  }

  case class STR_NOT_EQUALS(lhs: Exp[String], rhs: Exp[String]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = lhs.eval(env) != rhs.eval(env)
  }

  def fuzzyEquals(lhs: Double, rhs: Double): Boolean = {
    lhs.compare(rhs) == 0 || math.abs(lhs - rhs) < 0.001
  }

  case class EQUALS(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = fuzzyEquals(lhs.eval(env), rhs.eval(env))
  }

  case class NOT_EQUALS(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = !fuzzyEquals(lhs.eval(env), rhs.eval(env))
  }

  def numericCompare(env: Map[String, Any],
                     lhs: Exp[Double], rhs: Exp[Double],
                     comparator: (Double, Double) => Boolean): Boolean = {
    val lhsAns = lhs.eval(env)
    val rhsAns = rhs.eval(env)
    comparator(lhsAns, rhsAns)
  }

  case class LESSER_THAN(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = {
      numericCompare(env, lhs, rhs, (lhsAns, rhsAns) => !fuzzyEquals(lhsAns, rhsAns) && lhsAns < rhsAns)
    }
  }

  case class LESSER_THAN_EQ(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = {
      numericCompare(env, lhs, rhs, (lhsAns, rhsAns) => fuzzyEquals(lhsAns, rhsAns) || lhsAns <= rhsAns)
    }
  }

  case class GREATER_THAN(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = {
      numericCompare(env, lhs, rhs, (lhsAns, rhsAns) => !fuzzyEquals(lhsAns, rhsAns) && lhsAns > rhsAns)
    }
  }

  case class GREATER_THAN_EQ(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = {
      numericCompare(env, lhs, rhs, (lhsAns, rhsAns) => fuzzyEquals(lhsAns, rhsAns) || lhsAns > rhsAns)
    }
  }

  case class AND(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = lhs.eval(env) && rhs.eval(env)
  }

  case class OR(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = lhs.eval(env) || rhs.eval(env)
  }

  case class NOT(rhs: Exp[Boolean]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = !rhs.eval(env)
  }

  case class NARY_AND(rhs: Seq[Exp[Boolean]]) extends Exp[Boolean] {
    require(rhs.nonEmpty, "Args must be present")

    override def eval(env: Map[String, Any]): Boolean = rhs.forall(_.eval(env))
  }

  case class NARY_OR(rhs: Seq[Exp[Boolean]]) extends Exp[Boolean] {
    require(rhs.nonEmpty, "Args must be present")

    override def eval(env: Map[String, Any]): Boolean = rhs.exists(_.eval(env))
  }

  case class ADD(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Double] {
    override def eval(env: Map[String, Any]): Double = lhs.eval(env) + rhs.eval(env)
  }

  case class SUBTRACT(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Double] {
    override def eval(env: Map[String, Any]): Double = lhs.eval(env) - rhs.eval(env)
  }

  case class MULTIPLY(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Double] {
    override def eval(env: Map[String, Any]): Double = lhs.eval(env) * rhs.eval(env)
  }

  case class DIVIDE(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Double] {
    override def eval(env: Map[String, Any]): Double = lhs.eval(env) / rhs.eval(env)
  }

  case class IF(cond: Exp[Boolean], lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Double] {
    override def eval(env: Map[String, Any]): Double = {
      if (cond.eval(env)) lhs.eval(env) else rhs.eval(env)
    }
  }

  case class STR_SET_SYMBOL(key: String) extends Exp[Set[String]] {
    override def eval(env: Map[String, Any]): Set[String] = {
      env.get(key).collect {
        case set: Set[_] => collectStrings(set)
      }.getOrElse(Set.empty[String])
    }
  }

  case class STR_SET_CONTAINS(lhs: Exp[Set[String]], rhs: Exp[String]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = lhs.eval(env).contains(rhs.eval(env))
  }

}

class Expression(env: Map[String, Any]) {

  def eval[T](exp: Exp[T]): T = exp.eval(sanitiseValues(env))

  // explicit constructors in class for easier invocation form java
  def this() {
    this(Map.empty[String, Any])
  }

  def this(jmap: java.util.Map[java.lang.String, java.lang.Object]) {
    this(jmap2map(jmap))
  }
}
