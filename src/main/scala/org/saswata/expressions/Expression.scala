package org.saswata.expressions

import org.saswata.expressions.Expression.Exp
import org.saswata.expressions.ExpressionType._

// scalastyle:off number.of.types
object Expression {

  sealed trait Exp[R] {
    def eval(env: Map[String, Any]): R
  }

  final case class BOOL_SYMBOL(key: String) extends Exp[Boolean] with Symbol[Boolean] {

    override def eval(env: Map[String, Any]): Boolean =
      env
        .get(key)
        .fold(false)(Utils.coerceAny2Bool)
  }

  final case class STR_SYMBOL(key: String) extends Exp[String] with Symbol[String] {

    override def eval(env: Map[String, Any]): String = env.get(key).fold("")(Utils.coerceAny2String)
  }

  final case class STR_LITERAL(value: String) extends Exp[String] with Literal[String] {
    override def eval(env: Map[String, Any]): String = value
  }

  final case class NUM_SYMBOL(key: String) extends Exp[Double] with Symbol[Double] {
    override def eval(env: Map[String, Any]): Double =
      env
        .get(key)
        .fold(0.0)(Utils.coerceAny2Double)

  }

  final case class NUM_LITERAL(value: Double) extends Exp[Double] with Literal[Double] {
    override def eval(env: Map[String, Any]): Double = value
  }

  final case class STR_EQUALS(lhs: Exp[String], rhs: Exp[String])
      extends Exp[Boolean]
      with BinaryOperator[String, Boolean] {
    override def eval(env: Map[String, Any]): Boolean = lhs.eval(env) == rhs.eval(env)
  }

  final case class STR_NOT_EQUALS(lhs: Exp[String], rhs: Exp[String])
      extends Exp[Boolean]
      with BinaryOperator[String, Boolean] {
    override def eval(env: Map[String, Any]): Boolean = lhs.eval(env) != rhs.eval(env)
  }

  final case class EQUALS(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Boolean]
      with BinaryOperator[Double, Boolean] {
    override def eval(env: Map[String, Any]): Boolean =
      Utils.fuzzyEquals(lhs.eval(env), rhs.eval(env))
  }

  final case class NOT_EQUALS(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Boolean]
      with BinaryOperator[Double, Boolean] {
    override def eval(env: Map[String, Any]): Boolean =
      !Utils.fuzzyEquals(lhs.eval(env), rhs.eval(env))
  }

  final case class LESSER_THAN(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Boolean]
      with BinaryOperator[Double, Boolean] {
    override def eval(env: Map[String, Any]): Boolean =
      Utils.numericCompare(
        env,
        this,
        (lhsAns, rhsAns) => !Utils.fuzzyEquals(lhsAns, rhsAns) && lhsAns < rhsAns
      )
  }

  final case class LESSER_THAN_EQ(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Boolean]
      with BinaryOperator[Double, Boolean] {
    override def eval(env: Map[String, Any]): Boolean =
      Utils.numericCompare(
        env,
        this,
        (lhsAns, rhsAns) => Utils.fuzzyEquals(lhsAns, rhsAns) || lhsAns <= rhsAns
      )
  }

  final case class GREATER_THAN(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Boolean]
      with BinaryOperator[Double, Boolean] {
    override def eval(env: Map[String, Any]): Boolean =
      Utils.numericCompare(
        env,
        this,
        (lhsAns, rhsAns) => !Utils.fuzzyEquals(lhsAns, rhsAns) && lhsAns > rhsAns
      )
  }

  final case class GREATER_THAN_EQ(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Boolean]
      with BinaryOperator[Double, Boolean] {
    override def eval(env: Map[String, Any]): Boolean =
      Utils.numericCompare(
        env,
        this,
        (lhsAns, rhsAns) => Utils.fuzzyEquals(lhsAns, rhsAns) || lhsAns > rhsAns
      )
  }

  final case class AND(lhs: Exp[Boolean], rhs: Exp[Boolean])
      extends Exp[Boolean]
      with BinaryOperator[Boolean, Boolean] {
    override def eval(env: Map[String, Any]): Boolean = lhs.eval(env) && rhs.eval(env)
  }

  final case class OR(lhs: Exp[Boolean], rhs: Exp[Boolean])
      extends Exp[Boolean]
      with BinaryOperator[Boolean, Boolean] {
    override def eval(env: Map[String, Any]): Boolean = lhs.eval(env) || rhs.eval(env)
  }

  final case class NOT(rhs: Exp[Boolean])
      extends Exp[Boolean]
      with UnaryOperator[Boolean, Boolean] {
    override def eval(env: Map[String, Any]): Boolean = !rhs.eval(env)
  }

  final case class NARY_AND(rhs: Seq[Exp[Boolean]])
      extends Exp[Boolean]
      with NaryOperator[Boolean, Boolean] {
    require(rhs.nonEmpty, "Args must be present")

    override def eval(env: Map[String, Any]): Boolean = rhs.forall(_.eval(env))
  }

  final case class NARY_OR(rhs: Seq[Exp[Boolean]])
      extends Exp[Boolean]
      with NaryOperator[Boolean, Boolean] {
    require(rhs.nonEmpty, "Args must be present")

    override def eval(env: Map[String, Any]): Boolean = rhs.exists(_.eval(env))
  }

  final case class NEGATE(rhs: Exp[Double]) extends Exp[Double] with UnaryOperator[Double, Double] {
    override def eval(env: Map[String, Any]): Double = -1 * rhs.eval(env)
  }

  final case class ADD(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Double]
      with BinaryOperator[Double, Double] {
    override def eval(env: Map[String, Any]): Double = lhs.eval(env) + rhs.eval(env)
  }

  final case class SUBTRACT(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Double]
      with BinaryOperator[Double, Double] {
    override def eval(env: Map[String, Any]): Double = lhs.eval(env) - rhs.eval(env)
  }

  final case class MULTIPLY(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Double]
      with BinaryOperator[Double, Double] {
    override def eval(env: Map[String, Any]): Double = lhs.eval(env) * rhs.eval(env)
  }

  final case class DIVIDE(lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Double]
      with BinaryOperator[Double, Double] {
    override def eval(env: Map[String, Any]): Double = lhs.eval(env) / rhs.eval(env)
  }

  final case class MIN(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Double] {
    override def eval(env: Map[String, Any]): Double = math.min(lhs.eval(env), rhs.eval(env))
  }

  final case class MAX(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Double] {
    override def eval(env: Map[String, Any]): Double = math.max(lhs.eval(env), rhs.eval(env))
  }

  final case class IF(cond: Exp[Boolean], lhs: Exp[Double], rhs: Exp[Double])
      extends Exp[Double]
      with Condition[Double] {
    override def eval(env: Map[String, Any]): Double =
      if (cond.eval(env)) lhs.eval(env) else rhs.eval(env)
  }

  final case class STR_SET_SYMBOL(key: String) extends Exp[Set[String]] with Symbol[Set[String]] {

    override def eval(env: Map[String, Any]): Set[String] =
      env
        .get(key)
        .fold(Set.empty[String])(Utils.collectSet)
  }

  final case class STR_SET_CONTAINS(lhs: Exp[Set[String]], rhs: Exp[String]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = lhs.eval(env).contains(rhs.eval(env))
  }

}
// scalastyle:off number.of.types

final class Expression(env: Map[String, Any]) {

  def eval[T](exp: Exp[T]): T = exp.eval(Utils.sanitiseValues(env))

  // explicit constructors in class for easier invocation form java
  def this() {
    this(Map.empty[String, Any])
  }

  def this(jmap: java.util.Map[java.lang.String, java.lang.Object]) {
    this(Utils.jmap2map(jmap))
  }
}
