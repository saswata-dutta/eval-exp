package org.saswata.expressions

import org.saswata.expressions.Expression.Exp

object Evaluate {

  def fromJsonAsBool(env: Map[String, Any], json: String): Boolean = {
    val exp: Exp[Boolean] = JsonParser.parseBoolExp(json)
    new Expression(env).eval[Boolean](exp)
  }

  def fromJsonAsBool(
    jmap: java.util.Map[java.lang.String, java.lang.Object],
    json: String
  ): Boolean = {
    val exp: Exp[Boolean] = JsonParser.parseBoolExp(json)
    new Expression(jmap).eval[Boolean](exp)
  }

  def fromJsonAsNumber(env: Map[String, Any], json: String): Double = {
    val exp: Exp[Double] = JsonParser.parseNumExp(json)
    new Expression(env).eval[Double](exp)
  }

  def fromJsonAsNumber(
    jmap: java.util.Map[java.lang.String, java.lang.Object],
    json: String
  ): Double = {
    val exp: Exp[Double] = JsonParser.parseNumExp(json)
    new Expression(jmap).eval[Double](exp)
  }
}
