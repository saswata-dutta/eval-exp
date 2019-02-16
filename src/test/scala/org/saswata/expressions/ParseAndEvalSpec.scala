package org.saswata.expressions

import org.scalatest._

class ParseAndEvalSpec extends FlatSpec with Matchers {

  "The Parser and Evaluator" should "work for atomic ints" in {
    val json =
      """
        |{
        |	"type": "NUM_LITERAL",
        |	"value": 1
        |}
      """.stripMargin

    val exp = JsonParser.parseNumExp(json)
    Expression.eval[Double](exp, Map.empty[String, Any]) shouldEqual 1.0
  }

  it should "work for atomic decimals with 1 significant digit" in {
    val json =
      """
        |{
        |	"type": "NUM_LITERAL",
        |	"value": 100.1
        |}
      """.stripMargin

    val exp = JsonParser.parseNumExp(json)
    Expression.eval[Double](exp, Map.empty[String, Any]) shouldEqual 100.1
  }

  it should "work for atomic decimals with 2 significant digits" in {
    val json =
      """
        |{
        |	"type": "NUM_LITERAL",
        |	"value": 100.12
        |}
      """.stripMargin

    val exp = JsonParser.parseNumExp(json)
    Expression.eval[Double](exp, Map.empty[String, Any]) shouldEqual 100.12
  }

  it should "work for atomic strings" in {
    val json =
      """
        |{
        |	"type": "STR_LITERAL",
        |	"value": "HELLO"
        |}
      """.stripMargin

    val exp = JsonParser.parseStrAtom(JsonParser.parseJsonObj(json))
    exp.eval(Map.empty[String, Any]) shouldEqual "HELLO"
  }

}
