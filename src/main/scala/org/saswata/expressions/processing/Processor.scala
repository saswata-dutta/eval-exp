package org.saswata.expressions.processing

import org.saswata.expressions.Expression._

trait Processor[R] {
  def process(exp: BOOL_SYMBOL, env: Map[String, Any]): R

  def process(exp: STR_SYMBOL, env: Map[String, Any]): R

  def process(exp: STR_LITERAL, env: Map[String, Any]): R

  def process(exp: NUM_SYMBOL, env: Map[String, Any]): R

  def process(exp: NUM_LITERAL, env: Map[String, Any]): R

  def process(exp: STR_EQUALS, env: Map[String, Any]): R

  def process(exp: STR_NOT_EQUALS, env: Map[String, Any]): R

  def process(exp: EQUALS, env: Map[String, Any]): R

  def process(exp: NOT_EQUALS, env: Map[String, Any]): R

  def process(exp: LESSER_THAN, env: Map[String, Any]): R

  def process(exp: LESSER_THAN_EQ, env: Map[String, Any]): R

  def process(exp: GREATER_THAN, env: Map[String, Any]): R

  def process(exp: GREATER_THAN_EQ, env: Map[String, Any]): R

  def process(exp: AND, env: Map[String, Any]): R

  def process(exp: OR, env: Map[String, Any]): R

  def process(exp: NOT, env: Map[String, Any]): R

  def process(exp: NARY_AND, env: Map[String, Any]): R

  def process(exp: NARY_OR, env: Map[String, Any]): R

  def process(exp: NEGATE, env: Map[String, Any]): R

  def process(exp: ADD, env: Map[String, Any]): R

  def process(exp: SUBTRACT, env: Map[String, Any]): R

  def process(exp: MULTIPLY, env: Map[String, Any]): R

  def process(exp: DIVIDE, env: Map[String, Any]): R

  def process(exp: IF, env: Map[String, Any]): R

  def process(exp: STR_SET_SYMBOL, env: Map[String, Any]): R

  def process(exp: STR_SET_CONTAINS, env: Map[String, Any]): R
}
