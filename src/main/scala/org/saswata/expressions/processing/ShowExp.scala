package org.saswata.expressions.processing

import org.json4s.JsonDSL._
import org.json4s.{JArray, JBool, JDouble, JObject, JString, JValue}
import org.saswata.expressions.Expression._
import org.saswata.expressions.ExpressionType._

object ShowExp extends Processor[JObject] {

  def processExp[R](exp: Exp[R], env: Map[String, Any], evalValue: R => JValue): JObject =
    ("type"       -> exp.getClass.getSimpleName) ~
    ("eval-value" -> evalValue(exp.eval(env)))

  def processUnaryOp[R](
    exp: Exp[R] with UnaryOperator[_, R],
    env: Map[String, Any],
    evalValue: R => JValue
  ): JObject =
    processExp(exp, env, evalValue) ~
    ("rhs" -> Process(exp.rhs, env, ShowExp))

  def processBinaryOp[R](
    exp: Exp[R] with BinaryOperator[_, R],
    env: Map[String, Any],
    evalValue: R => JValue
  ): JObject =
    processExp(exp, env, evalValue) ~
    ("lhs" -> Process(exp.lhs, env, ShowExp)) ~
    ("rhs" -> Process(exp.rhs, env, ShowExp))

  def processNaryOp[R](
    exp: Exp[R] with NaryOperator[_, R],
    env: Map[String, Any],
    evalValue: R => JValue
  ): JObject =
    processExp(exp, env, evalValue) ~
    ("rhs" -> JArray(exp.rhs.map(it => Process[JObject](it, env, ShowExp)).toList))

  def processSymbol[R](exp: Exp[R] with Symbol[R], evalValue: R => JValue): JObject =
    processExp(exp, Map.empty, evalValue) ~
    ("key" -> exp.key)

  def processLiteral[R](exp: Exp[R] with Literal[R], evalValue: R => JValue): JObject =
    processExp(exp, Map.empty, evalValue) ~
    ("value" -> evalValue(exp.value))

  def processCondition[R](
    exp: Exp[R] with Condition[R],
    env: Map[String, Any],
    evalValue: R => JValue
  ): JObject =
    processExp(exp, env, evalValue) ~
    ("cond" -> Process[JObject](exp.cond, env, ShowExp)) ~
    ("lhs"  -> Process[JObject](exp.lhs, env, ShowExp)) ~
    ("rhs"  -> Process[JObject](exp.rhs, env, ShowExp))

  private val asBoolean = (it: Boolean) => JBool(it)
  private val asString = (it: String) => JString(it)
  private val asDouble = (it: Double) => JDouble(it)

  override def process(exp: BOOL_SYMBOL, env: Map[String, Any]): JObject =
    processSymbol(exp, asBoolean)

  override def process(exp: STR_SYMBOL, env: Map[String, Any]): JObject =
    processSymbol(exp, asString)

  override def process(exp: STR_LITERAL, env: Map[String, Any]): JObject =
    processLiteral(exp, asString)

  override def process(exp: NUM_SYMBOL, env: Map[String, Any]): JObject =
    processSymbol(exp, asDouble)

  override def process(exp: NUM_LITERAL, env: Map[String, Any]): JObject =
    processLiteral(exp, asDouble)

  override def process(exp: STR_EQUALS, env: Map[String, Any]): JObject =
    processBinaryOp(exp, env, asBoolean)

  override def process(exp: STR_NOT_EQUALS, env: Map[String, Any]): JObject =
    processBinaryOp(exp, env, asBoolean)

  override def process(exp: EQUALS, env: Map[String, Any]): JObject =
    processBinaryOp(exp, env, asBoolean)

  override def process(exp: NOT_EQUALS, env: Map[String, Any]): JObject =
    processBinaryOp(exp, env, asBoolean)

  override def process(exp: LESSER_THAN, env: Map[String, Any]): JObject =
    processBinaryOp(exp, env, asBoolean)

  override def process(exp: LESSER_THAN_EQ, env: Map[String, Any]): JObject =
    processBinaryOp(exp, env, asBoolean)

  override def process(exp: GREATER_THAN, env: Map[String, Any]): JObject =
    processBinaryOp(exp, env, asBoolean)

  override def process(exp: GREATER_THAN_EQ, env: Map[String, Any]): JObject =
    processBinaryOp(exp, env, asBoolean)

  override def process(exp: AND, env: Map[String, Any]): JObject =
    processBinaryOp(exp, env, asBoolean)

  override def process(exp: OR, env: Map[String, Any]): JObject =
    processBinaryOp(exp, env, asBoolean)

  override def process(exp: NOT, env: Map[String, Any]): JObject =
    processUnaryOp(exp, env, asBoolean)

  override def process(exp: NARY_AND, env: Map[String, Any]): JObject =
    processNaryOp(exp, env, asBoolean)

  override def process(exp: NARY_OR, env: Map[String, Any]): JObject =
    processNaryOp(exp, env, asBoolean)

  override def process(exp: ADD, env: Map[String, Any]): JObject =
    processBinaryOp(exp, env, asDouble)

  override def process(exp: SUBTRACT, env: Map[String, Any]): JObject =
    processBinaryOp(exp, env, asDouble)

  override def process(exp: MULTIPLY, env: Map[String, Any]): JObject =
    processBinaryOp(exp, env, asDouble)

  override def process(exp: DIVIDE, env: Map[String, Any]): JObject =
    processBinaryOp(exp, env, asDouble)

  override def process(exp: IF, env: Map[String, Any]): JObject =
    processCondition(exp, env, asDouble)

  override def process(exp: STR_SET_SYMBOL, env: Map[String, Any]): JObject =
    ("type"       -> exp.getClass.getSimpleName) ~
    ("eval-value" -> JArray(exp.eval(env).map(JString).toList)) ~
    ("key"        -> exp.key)

  override def process(exp: STR_SET_CONTAINS, env: Map[String, Any]): JObject =
    processExp(exp, env, asBoolean) ~
    ("lhs" -> Process[JObject](exp.lhs, env, ShowExp)) ~
    ("rhs" -> Process[JObject](exp.rhs, env, ShowExp))

  override def process(exp: NEGATE, env: Map[String, Any]): JObject =
    processUnaryOp(exp, env, asDouble)
}
