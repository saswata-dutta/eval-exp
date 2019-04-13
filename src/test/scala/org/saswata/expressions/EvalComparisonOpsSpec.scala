package org.saswata.expressions

import org.saswata.expressions.Expression._
import org.scalatest.{FlatSpec, Matchers}

class EvalComparisonOpsSpec extends FlatSpec with Matchers {

  "The Evaluator" should "fuzzy compare numbers" in {
    val n1 = NUM_SYMBOL("n1")
    val n2 = NUM_SYMBOL("n2")

    val eq = EQUALS(n1, n2)
    eq.eval(Map.empty) shouldEqual true
    eq.eval(Map("n1" -> 1.0, "n2"   -> 1.0)) shouldEqual true
    eq.eval(Map("n1" -> 1.0, "n2"   -> 1.001)) shouldEqual true
    eq.eval(Map("n1" -> 1.01, "n2"  -> 1.011)) shouldEqual true
    eq.eval(Map("n1" -> 1.01, "n2"  -> 1.009)) shouldEqual false
    eq.eval(Map("n1" -> 1.009, "n2" -> 1.01)) shouldEqual false

    val ne = NOT_EQUALS(n1, n2)
    ne.eval(Map.empty) shouldEqual false
    ne.eval(Map("n1" -> 1.0, "n2"   -> 1.0)) shouldEqual false
    ne.eval(Map("n1" -> 1.0, "n2"   -> 1.001)) shouldEqual false
    ne.eval(Map("n1" -> 1.01, "n2"  -> 1.011)) shouldEqual false
    ne.eval(Map("n1" -> 1.01, "n2"  -> 1.009)) shouldEqual true
    ne.eval(Map("n1" -> 1.009, "n2" -> 1.01)) shouldEqual true

    val lt = LESSER_THAN(n1, n2)
    lt.eval(Map.empty) shouldEqual false
    lt.eval(Map("n1" -> 1.0, "n2"   -> 1.0)) shouldEqual false
    lt.eval(Map("n1" -> 1.0, "n2"   -> 1.001)) shouldEqual false
    lt.eval(Map("n1" -> 1.01, "n2"  -> 1.011)) shouldEqual false
    lt.eval(Map("n1" -> 1.01, "n2"  -> 1.009)) shouldEqual false
    lt.eval(Map("n1" -> 1.009, "n2" -> 1.01)) shouldEqual true

    val le = LESSER_THAN_EQ(n1, n2)
    le.eval(Map.empty) shouldEqual true
    le.eval(Map("n1" -> 1.0, "n2"   -> 1.0)) shouldEqual true
    le.eval(Map("n1" -> 1.0, "n2"   -> 1.001)) shouldEqual true
    le.eval(Map("n1" -> 1.01, "n2"  -> 1.011)) shouldEqual true
    le.eval(Map("n1" -> 1.01, "n2"  -> 1.009)) shouldEqual false
    le.eval(Map("n1" -> 1.009, "n2" -> 1.01)) shouldEqual true

    val gt = GREATER_THAN(n1, n2)
    gt.eval(Map.empty) shouldEqual false
    gt.eval(Map("n1" -> 1.0, "n2"   -> 1.0)) shouldEqual false
    gt.eval(Map("n1" -> 1.0, "n2"   -> 1.001)) shouldEqual false
    gt.eval(Map("n1" -> 1.01, "n2"  -> 1.011)) shouldEqual false
    gt.eval(Map("n1" -> 1.01, "n2"  -> 1.009)) shouldEqual true
    gt.eval(Map("n1" -> 1.009, "n2" -> 1.01)) shouldEqual false

    val ge = GREATER_THAN_EQ(n1, n2)
    ge.eval(Map.empty) shouldEqual true
    ge.eval(Map("n1" -> 1.0, "n2"   -> 1.0)) shouldEqual true
    ge.eval(Map("n1" -> 1.0, "n2"   -> 1.001)) shouldEqual true
    ge.eval(Map("n1" -> 1.01, "n2"  -> 1.011)) shouldEqual true
    ge.eval(Map("n1" -> 1.01, "n2"  -> 1.009)) shouldEqual true
    ge.eval(Map("n1" -> 1.009, "n2" -> 1.01)) shouldEqual false
  }

  it should "compute multi operand AND and OR" in {
    val f1 = BOOL_SYMBOL("f1")
    val f2 = BOOL_SYMBOL("f2")
    val f3 = BOOL_SYMBOL("f3")
    val f4 = BOOL_SYMBOL("f4")
    val operands = Seq(f1, f2, f3, f4)

    val andEx = NARY_AND(operands)
    andEx.eval(Map("f1" -> true, "f2" -> true, "f3" -> true, "f4" -> true)) shouldEqual true
    andEx.eval(Map("f1" -> true, "f2" -> true, "f3" -> true)) shouldEqual false

    val orExp = NARY_OR(operands)
    orExp.eval(Map("f1" -> true, "f2" -> true, "f3" -> true, "f4" -> true)) shouldEqual true
    orExp.eval(Map("f4" -> true)) shouldEqual true
    orExp.eval(Map()) shouldEqual false
  }

  it should "disallow creation of zero Args N-ary AND/OR" in {
    val thrown1 = intercept[IllegalArgumentException](NARY_AND(Seq.empty))
    assert(thrown1.getMessage contains "Args must be present")

    val thrown2 = intercept[IllegalArgumentException](NARY_OR(Seq.empty))
    assert(thrown2.getMessage contains "Args must be present")
  }
}
