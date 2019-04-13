package org.saswata.expressions

import org.saswata.expressions.Expression.Exp
import org.saswata.expressions.ExpressionType._
import org.saswata.expressions.Utils._

object Expression {

  sealed trait Exp[R] {
    def eval(env: Map[String, Any]): R
  }

  case class BOOL_SYMBOL(key: String) extends Exp[Boolean] with Symbol[Boolean] {
    override def eval(env: Map[String, Any]): Boolean =
      env
        .get(key)
        .collect {
          case b: Boolean => b
          case s: String  => s.toBoolean
        }
        .getOrElse(false)
  }

  case class STR_SYMBOL(key: String) extends Exp[String] with Symbol[String] {
    override def eval(env: Map[String, Any]): String = env.get(key).map(_.toString).getOrElse("")
  }

  case class STR_LITERAL(value: String) extends Exp[String] with Literal[String] {
    override def eval(env: Map[String, Any]): String = value
  }

  case class NUM_SYMBOL(key: String) extends Exp[Double] with Symbol[Double] {
    override def eval(env: Map[String, Any]): Double =
      env
        .get(key)
        .collect {
          case n: java.lang.Number => n.doubleValue()
          case s: String           => s.toDouble
        }
        .getOrElse(0.0)
  }

  case class NUM_LITERAL(value: Double) extends Exp[Double] with Literal[Double] {
    override def eval(env: Map[String, Any]): Double = value
  }

  case class STR_EQUALS(lhs: Exp[String], rhs: Exp[String])
      extends Exp[Boolean]
      with BinaryOperator[String, Boolean] {
    override def eval(env: Map[String, Any]): Boolean = lhs.eval(env) == rhs.eval(env)
  }

  case class STR_NOT_EQUALS(lhs: Exp[String], rhs: Exp[String])
      extends Exp[Boolean]
      with BinaryOperator[String, Boolean] {
    override def eval(env: Map[String, Any]): Boolean = lhs.eval(env) != rhs.eval(env)
  }

  case class EQUALS(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Boolean]
      with BinaryOperator[Double, Boolean] {
    override def eval(env: Map[String, Any]): Boolean = fuzzyEquals(lhs.eval(env), rhs.eval(env))
  }

  case class NOT_EQUALS(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Boolean]
      with BinaryOperator[Double, Boolean] {
    override def eval(env: Map[String, Any]): Boolean = !fuzzyEquals(lhs.eval(env), rhs.eval(env))
  }

  case class LESSER_THAN(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Boolean]
      with BinaryOperator[Double, Boolean] {
    override def eval(env: Map[String, Any]): Boolean =
      numericCompare(
        env,
        lhs,
        rhs,
        (lhsAns, rhsAns) => !fuzzyEquals(lhsAns, rhsAns) && lhsAns < rhsAns
      )
  }

  case class LESSER_THAN_EQ(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Boolean]
      with BinaryOperator[Double, Boolean] {
    override def eval(env: Map[String, Any]): Boolean =
      numericCompare(
        env,
        lhs,
        rhs,
        (lhsAns, rhsAns) => fuzzyEquals(lhsAns, rhsAns) || lhsAns <= rhsAns
      )
  }

  case class GREATER_THAN(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Boolean]
      with BinaryOperator[Double, Boolean] {
    override def eval(env: Map[String, Any]): Boolean =
      numericCompare(
        env,
        lhs,
        rhs,
        (lhsAns, rhsAns) => !fuzzyEquals(lhsAns, rhsAns) && lhsAns > rhsAns
      )
  }

  case class GREATER_THAN_EQ(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Boolean]
      with BinaryOperator[Double, Boolean] {
    override def eval(env: Map[String, Any]): Boolean =
      numericCompare(
        env,
        lhs,
        rhs,
        (lhsAns, rhsAns) => fuzzyEquals(lhsAns, rhsAns) || lhsAns > rhsAns
      )
  }

  case class AND(lhs: Exp[Boolean], rhs: Exp[Boolean])
      extends Exp[Boolean]
      with BinaryOperator[Boolean, Boolean] {
    override def eval(env: Map[String, Any]): Boolean = lhs.eval(env) && rhs.eval(env)
  }

  case class OR(lhs: Exp[Boolean], rhs: Exp[Boolean])
      extends Exp[Boolean]
      with BinaryOperator[Boolean, Boolean] {
    override def eval(env: Map[String, Any]): Boolean = lhs.eval(env) || rhs.eval(env)
  }

  case class NOT(rhs: Exp[Boolean]) extends Exp[Boolean] with UnaryOperator[Boolean, Boolean] {
    override def eval(env: Map[String, Any]): Boolean = !rhs.eval(env)
  }

  case class NARY_AND(rhs: Seq[Exp[Boolean]])
      extends Exp[Boolean]
      with NaryOperator[Boolean, Boolean] {
    require(rhs.nonEmpty, "Args must be present")

    override def eval(env: Map[String, Any]): Boolean = rhs.forall(_.eval(env))
  }

  case class NARY_OR(rhs: Seq[Exp[Boolean]])
      extends Exp[Boolean]
      with NaryOperator[Boolean, Boolean] {
    require(rhs.nonEmpty, "Args must be present")

    override def eval(env: Map[String, Any]): Boolean = rhs.exists(_.eval(env))
  }

  case class NEGATE(rhs: Exp[Double]) extends Exp[Double] with UnaryOperator[Double, Double] {
    override def eval(env: Map[String, Any]): Double = -1 * rhs.eval(env)
  }

  case class ADD(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Double]
      with BinaryOperator[Double, Double] {
    override def eval(env: Map[String, Any]): Double = lhs.eval(env) + rhs.eval(env)
  }

  case class SUBTRACT(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Double]
      with BinaryOperator[Double, Double] {
    override def eval(env: Map[String, Any]): Double = lhs.eval(env) - rhs.eval(env)
  }

  case class MULTIPLY(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Double]
      with BinaryOperator[Double, Double] {
    override def eval(env: Map[String, Any]): Double = lhs.eval(env) * rhs.eval(env)
  }

  case class DIVIDE(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Double]
      with BinaryOperator[Double, Double] {
    override def eval(env: Map[String, Any]): Double = lhs.eval(env) / rhs.eval(env)
  }

  case class MIN(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Double] {
    override def eval(env: Map[String, Any]): Double = math.min(lhs.eval(env), rhs.eval(env))
  }

  case class MAX(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Double] {
    override def eval(env: Map[String, Any]): Double = math.max(lhs.eval(env), rhs.eval(env))
  }

  case class IF(cond: Exp[Boolean], lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Double]
      with Condition[Double] {
    override def eval(env: Map[String, Any]): Double =
      if (cond.eval(env)) lhs.eval(env) else rhs.eval(env)
  }

  case class STR_SET_SYMBOL(key: String) extends Exp[Set[String]] with Symbol[Set[String]] {
    override def eval(env: Map[String, Any]): Set[String] =
      env
        .get(key)
        .collect {
          case set: Set[_] => collectStrings(set)
        }
        .getOrElse(Set.empty[String])
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
