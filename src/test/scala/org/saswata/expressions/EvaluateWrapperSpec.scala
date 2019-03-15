package org.saswata.expressions

import java.util

import org.scalatest._

class EvaluateWrapperSpec extends FlatSpec with Matchers {
  "The Evaluator Wrapper" should "evaluate numeric expressions" in {
    val json =
      """
        |{
        |  "type": "NUM_SYMBOL",
        |  "key": "num"
        |}
      """.stripMargin

    Evaluate.fromJsonAsNumber(Map.empty[String, Any], json) shouldEqual 0
    Evaluate.fromJsonAsNumber(Map("num" -> 1), json) shouldEqual 1.0
    Evaluate.fromJsonAsNumber(Map("num" -> "2"), json) shouldEqual 2.0
    Evaluate.fromJsonAsNumber(Map("num" -> null), json) shouldEqual 0

    val jmap: java.util.Map[java.lang.String, java.lang.Object] = new util.HashMap()
    Evaluate.fromJsonAsNumber(jmap, json) shouldEqual 0
    jmap.put("num", Integer.valueOf(1))
    Evaluate.fromJsonAsNumber(jmap, json) shouldEqual 1.0
    jmap.put("num", "2")
    Evaluate.fromJsonAsNumber(jmap, json) shouldEqual 2.0
    jmap.put("num", java.lang.Long.MAX_VALUE.toString)
    Evaluate.fromJsonAsNumber(jmap, json) shouldEqual java.lang.Long.MAX_VALUE
    jmap.put("num", "10.25")
    Evaluate.fromJsonAsNumber(jmap, json) shouldEqual 10.25
    jmap.put("num", null)
    Evaluate.fromJsonAsNumber(jmap, json) shouldEqual 0
  }

  it should "evaluate bool expressions" in {
    val json =
      """
        |{
        |  "type": "BOOL_SYMBOL",
        |  "key": "flag"
        |}
      """.stripMargin

    Evaluate.fromJsonAsBool(Map.empty[String, Any], json) shouldEqual false
    Evaluate.fromJsonAsBool(Map("num" -> 1), json) shouldEqual false
    Evaluate.fromJsonAsBool(Map("flag" -> null), json) shouldEqual false
    Evaluate.fromJsonAsBool(Map("flag" -> false), json) shouldEqual false
    Evaluate.fromJsonAsBool(Map("flag" -> true), json) shouldEqual true
    Evaluate.fromJsonAsBool(Map("flag" -> "true"), json) shouldEqual true

    val jmap: java.util.Map[java.lang.String, java.lang.Object] = new util.HashMap()
    Evaluate.fromJsonAsBool(jmap, json) shouldEqual false
    jmap.put("num", "2")
    Evaluate.fromJsonAsBool(jmap, json) shouldEqual false
    jmap.put("flag", "true")
    Evaluate.fromJsonAsBool(jmap, json) shouldEqual true
    jmap.put("flag", "false")
    Evaluate.fromJsonAsBool(jmap, json) shouldEqual false
    jmap.put("flag", java.lang.Boolean.TRUE)
    Evaluate.fromJsonAsBool(jmap, json) shouldEqual true
    jmap.put("flag", null)
    Evaluate.fromJsonAsBool(jmap, json) shouldEqual false
  }

  it should "evaluate set_contains operations" in {

  }
}
