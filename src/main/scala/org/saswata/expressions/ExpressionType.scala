package org.saswata.expressions

import org.saswata.expressions.Expression.Exp

object ExpressionType {

  trait BinaryOperator[T, R] {
    def lhs: Exp[T]

    def rhs: Exp[T]

    def eval(env: Map[String, Any]): R
  }

  trait UnaryOperator[T, R] {
    def rhs: Exp[T]

    def eval(env: Map[String, Any]): R
  }

  trait NaryOperator[T, R] {
    def rhs: Seq[Exp[T]]

    def eval(env: Map[String, Any]): R
  }

  trait Symbol[R] {
    def key: String

    def eval(env: Map[String, Any]): R
  }

  trait Literal[R] {
    def value: R

    def eval(env: Map[String, Any]): R
  }

  trait Condition[R] {
    def cond: Exp[Boolean]

    def lhs: Exp[R]

    def rhs: Exp[R]

    def eval(env: Map[String, Any]): R
  }

}
