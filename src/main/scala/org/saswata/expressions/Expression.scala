package org.saswata.expressions

object Expression {

  private val NUM_SCALE: Double = 100.0

  private def round(value: Double): Double = math.round(value * NUM_SCALE) / NUM_SCALE

  sealed trait Exp[R] {
    def eval(env: Map[String, Any]): R
  }

  def eval[T](exp: Exp[T], env: Map[String, Any]): T = exp.eval(env)

  case class STR_SYMBOL(key: String) extends Exp[String] {
    override def eval(env: Map[String, Any]): String = env.get(key).map(_.asInstanceOf[String]).getOrElse("")
  }

  case class STR_LITERAL(value: String) extends Exp[String] {
    override def eval(env: Map[String, Any]): String = value
  }

  case class NUM_SYMBOL(key: String) extends Exp[Double] {
    override def eval(env: Map[String, Any]): Double = round(env.get(key).map(_.asInstanceOf[Double]).getOrElse(0.0))
  }

  case class NUM_LITERAL(value: Double) extends Exp[Double] {

    private val rounded: Double = round(value)

    override def eval(env: Map[String, Any]): Double = rounded
  }

  case class STR_EQUALS(lhs: Exp[String], rhs: Exp[String]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = lhs.eval(env) == rhs.eval(env)
  }

  case class STR_NOT_EQUALS(lhs: Exp[String], rhs: Exp[String]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = lhs.eval(env) != rhs.eval(env)
  }

  case class EQUALS(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = round(lhs.eval(env)) == round(rhs.eval(env))
  }

  case class NOT_EQUALS(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = round(lhs.eval(env)) != round(rhs.eval(env))
  }

  case class LESSER_THAN(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = round(lhs.eval(env)) < round(rhs.eval(env))
  }

  case class LESSER_THAN_EQ(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = round(lhs.eval(env)) <= round(rhs.eval(env))
  }

  case class GREATER_THAN(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = round(lhs.eval(env)) > round(rhs.eval(env))
  }

  case class GREATER_THAN_EQ(lhs: Exp[Double], rhs: Exp[Double]) extends Exp[Boolean] {
    override def eval(env: Map[String, Any]): Boolean = round(lhs.eval(env)) >= round(rhs.eval(env))
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

}
