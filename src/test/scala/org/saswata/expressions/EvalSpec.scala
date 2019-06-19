package org.saswata.expressions

import org.saswata.expressions.Expression._
import org.scalatest.{FlatSpec, Matchers}

class EvalSpec extends FlatSpec with Matchers {
  "The Evaluator" should "short circuit AND" in {
    val denom = NUM_LITERAL(0.0)
    val nume = NUM_LITERAL(1.0)
    val threshold = NUM_LITERAL(0.5)
    val passed = GREATER_THAN(DIVIDE(nume, denom), threshold)

    val guard = NOT_EQUALS(denom, NUM_LITERAL(0.0))
    val cond = AND(guard, passed)

    cond.eval(Map.empty) shouldEqual false
  }

  it should "short circuit OR" in {
    val denom = NUM_LITERAL(0.0)
    val nume = NUM_LITERAL(1.0)
    val threshold = NUM_LITERAL(0.5)
    val passed = GREATER_THAN(DIVIDE(nume, denom), threshold)

    val guard = EQUALS(denom, NUM_LITERAL(0.0))
    val cond = OR(guard, passed)

    cond.eval(Map.empty) shouldEqual true
  }
}
