package org.saswata.expressions

import org.saswata.expressions.ExpressionType.BinaryOperator

import scala.collection.JavaConverters._

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object Utils {

  def jmap2map(jmap: java.util.Map[java.lang.String, java.lang.Object]): Map[String, Any] =
    jmap.asScala.toMap

  def collectStrings(set: Set[_]): Set[String] = set.collect { case s: String => s }

  def sanitiseValues(env: Map[String, Any]): Map[String, Any] =
    env.filter(_._2 != null).collect {
      case (k: String, b: Boolean)                   => (k, b)
      case (k: String, n: java.lang.Number)          => (k, n.doubleValue())
      case (k: String, s: String)                    => (k, s)
      case (k: String, jIter: java.lang.Iterable[_]) => (k, collectStrings(jIter.asScala.toSet))
      case (k: String, seq: Seq[_])                  => (k, collectStrings(seq.toSet))
      case (k: String, set: Set[_])                  => (k, collectStrings(set))
    }

  def numericCompare(
    env: Map[String, Any],
    op: BinaryOperator[Double, Boolean],
    comparator: (Double, Double) => Boolean
  ): Boolean = {
    val lhsAns = op.lhs.eval(env)
    val rhsAns = op.rhs.eval(env)
    comparator(lhsAns, rhsAns)
  }

  def fuzzyEquals(lhs: Double, rhs: Double): Boolean =
    lhs.compare(rhs) == 0 || math.abs(lhs - rhs) < 0.001

  val coerceAny2Double: PartialFunction[Any, Double] = {
    case n: java.lang.Number => n.doubleValue()
    case s: String           => s.toDouble
  }

  val coerceAny2Bool: PartialFunction[Any, Boolean] = {
    case b: Boolean => b
    case s: String  => s.toBoolean
  }

  val coerceAny2String: PartialFunction[Any, String] = {
    case s: String => s
    case any       => any.toString
  }

  val collectSet: PartialFunction[Any, Set[String]] = { case set: Set[_] => collectStrings(set) }
}
