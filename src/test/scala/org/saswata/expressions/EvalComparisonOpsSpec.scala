package org.saswata.expressions

import org.saswata.expressions.Expression._
import org.scalatest.{FlatSpec, Matchers}

class EvalComparisonOpsSpec extends FlatSpec with Matchers {

  "The Evaluator" should "fuzzy compare numbers" in {
    val n1 = NUM_SYMBOL("n1")
    val n2 = NUM_SYMBOL("n2")

    val eq = EQUALS(n1, n2)
    eq.eval(Map.empty) shouldEqual true
    eq.eval(Map("n1" -> 1.0, "n2" -> 1.0)) shouldEqual true
    eq.eval(Map("n1" -> 1.0, "n2" -> 1.001)) shouldEqual true
    eq.eval(Map("n1" -> 1.01, "n2" -> 1.011)) shouldEqual true
    eq.eval(Map("n1" -> 1.01, "n2" -> 1.009)) shouldEqual false
    eq.eval(Map("n1" -> 1.009, "n2" -> 1.01)) shouldEqual false

    val ne = NOT_EQUALS(n1, n2)
    ne.eval(Map.empty) shouldEqual false
    ne.eval(Map("n1" -> 1.0, "n2" -> 1.0)) shouldEqual false
    ne.eval(Map("n1" -> 1.0, "n2" -> 1.001)) shouldEqual false
    ne.eval(Map("n1" -> 1.01, "n2" -> 1.011)) shouldEqual false
    ne.eval(Map("n1" -> 1.01, "n2" -> 1.009)) shouldEqual true
    ne.eval(Map("n1" -> 1.009, "n2" -> 1.01)) shouldEqual true

    val lt = LESSER_THAN(n1, n2)
    lt.eval(Map.empty) shouldEqual false
    lt.eval(Map("n1" -> 1.0, "n2" -> 1.0)) shouldEqual false
    lt.eval(Map("n1" -> 1.0, "n2" -> 1.001)) shouldEqual false
    lt.eval(Map("n1" -> 1.01, "n2" -> 1.011)) shouldEqual false
    lt.eval(Map("n1" -> 1.01, "n2" -> 1.009)) shouldEqual false
    lt.eval(Map("n1" -> 1.009, "n2" -> 1.01)) shouldEqual true

    val le = LESSER_THAN_EQ(n1, n2)
    le.eval(Map.empty) shouldEqual true
    le.eval(Map("n1" -> 1.0, "n2" -> 1.0)) shouldEqual true
    le.eval(Map("n1" -> 1.0, "n2" -> 1.001)) shouldEqual true
    le.eval(Map("n1" -> 1.01, "n2" -> 1.011)) shouldEqual true
    le.eval(Map("n1" -> 1.01, "n2" -> 1.009)) shouldEqual false
    le.eval(Map("n1" -> 1.009, "n2" -> 1.01)) shouldEqual true

    val gt = GREATER_THAN(n1, n2)
    gt.eval(Map.empty) shouldEqual false
    gt.eval(Map("n1" -> 1.0, "n2" -> 1.0)) shouldEqual false
    gt.eval(Map("n1" -> 1.0, "n2" -> 1.001)) shouldEqual false
    gt.eval(Map("n1" -> 1.01, "n2" -> 1.011)) shouldEqual false
    gt.eval(Map("n1" -> 1.01, "n2" -> 1.009)) shouldEqual true
    gt.eval(Map("n1" -> 1.009, "n2" -> 1.01)) shouldEqual false

    val ge = GREATER_THAN_EQ(n1, n2)
    ge.eval(Map.empty) shouldEqual true
    ge.eval(Map("n1" -> 1.0, "n2" -> 1.0)) shouldEqual true
    ge.eval(Map("n1" -> 1.0, "n2" -> 1.001)) shouldEqual true
    ge.eval(Map("n1" -> 1.01, "n2" -> 1.011)) shouldEqual true
    ge.eval(Map("n1" -> 1.01, "n2" -> 1.009)) shouldEqual true
    ge.eval(Map("n1" -> 1.009, "n2" -> 1.01)) shouldEqual false
  }

}
