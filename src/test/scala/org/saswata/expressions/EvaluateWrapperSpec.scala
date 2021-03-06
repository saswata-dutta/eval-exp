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
    Evaluate.fromJsonAsBool(Map("num"  -> 1), json) shouldEqual false
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

  it should "parse and evaluate String set contains" in {
    val json =
      """
        |{
        |    "lhs": {
        |        "key": "tags",
        |        "type": "STR_SET_SYMBOL"
        |    },
        |    "rhs": {
        |        "key": "tag",
        |        "type": "STR_SYMBOL"
        |    },
        |    "type": "STR_SET_CONTAINS"
        |}
      """.stripMargin

    Evaluate.fromJsonAsBool(Map.empty[String, Any], json) shouldEqual false
    Evaluate.fromJsonAsBool(Map("tag" -> "x"), json) shouldEqual false
    Evaluate.fromJsonAsBool(Map("tag" -> 1L), json) shouldEqual false
    Evaluate.fromJsonAsBool(Map("tag" -> 1.25), json) shouldEqual false

    Evaluate.fromJsonAsBool(Map("bla"  -> Set(1, 2, 3)), json) shouldEqual false
    Evaluate.fromJsonAsBool(Map("tags" -> Set(1, 2, 3)), json) shouldEqual false
    Evaluate.fromJsonAsBool(Map("tags" -> Set("a", "b", "c")), json) shouldEqual false
    Evaluate.fromJsonAsBool(Map("tags" -> Seq("a", "b", "c")), json) shouldEqual false

    Evaluate.fromJsonAsBool(Map("tags" -> Set.empty[String], "tag"  -> "a"), json) shouldEqual false
    Evaluate.fromJsonAsBool(Map("tags" -> Seq.empty[String], "tag"  -> "a"), json) shouldEqual false
    Evaluate.fromJsonAsBool(Map("tags" -> Set(1, 2, 3), "tag"       -> "a"), json) shouldEqual false
    Evaluate.fromJsonAsBool(Map("tags" -> Set("a", "b", "c"), "tag" -> 1), json) shouldEqual false
    Evaluate.fromJsonAsBool(Map("tags" -> Seq("a", "b", "c"), "tag" -> "x"), json) shouldEqual false

    Evaluate.fromJsonAsBool(Map("tags" -> Set(1, 2, 3), "tag"       -> 1), json) shouldEqual false
    Evaluate.fromJsonAsBool(Map("tags" -> Seq(1, 2, 3), "tag"       -> 1), json) shouldEqual false
    Evaluate.fromJsonAsBool(Map("tags" -> Seq("a", "b", "c"), "tag" -> "c"), json) shouldEqual true

    val jmap: java.util.Map[java.lang.String, java.lang.Object] = new util.HashMap()
    Evaluate.fromJsonAsBool(jmap, json) shouldEqual false

    jmap.put("tag", java.lang.Integer.valueOf(1))
    Evaluate.fromJsonAsBool(jmap, json) shouldEqual false

    jmap.put("tags", java.util.Arrays.asList(1, 2, 3))
    Evaluate.fromJsonAsBool(jmap, json) shouldEqual false

    jmap.put("tags", java.util.Arrays.asList("1.0", 2, 3))
    Evaluate.fromJsonAsBool(jmap, json) shouldEqual true

    jmap.put("tag", "x")
    jmap.put("tags", java.util.Collections.singleton("y"))
    Evaluate.fromJsonAsBool(jmap, json) shouldEqual false
    jmap.put("tags", java.util.Collections.singleton("x"))
    Evaluate.fromJsonAsBool(jmap, json) shouldEqual true

    jmap.put("tag", null)
    Evaluate.fromJsonAsBool(jmap, json) shouldEqual false

    jmap.put("tag", "x")
    jmap.put("tags", new java.util.HashSet(java.util.Arrays.asList("a", "b", "x")))
    Evaluate.fromJsonAsBool(jmap, json) shouldEqual true
    jmap.put("tags", java.util.Collections.emptyList())
    Evaluate.fromJsonAsBool(jmap, json) shouldEqual false
    jmap.put("tags", java.util.Collections.emptySet())
    Evaluate.fromJsonAsBool(jmap, json) shouldEqual false
    jmap.put("tags", null)
    Evaluate.fromJsonAsBool(jmap, json) shouldEqual false
  }

  it should "parse and evaluate min" in {
    val json = "{\"type\":\"MIN\",\"lhs\":{\"type\":\"NUM_LITERAL\",\"value\":100}," +
      "\"rhs\":{\"type\":\"NUM_SYMBOL\",\"key\":\"x\"}}"

    Evaluate.fromJsonAsNumber(Map.empty[String, Any], json) shouldEqual 0.0
    Evaluate.fromJsonAsNumber(Map("x" -> 10), json) shouldEqual 10.0
    Evaluate.fromJsonAsNumber(Map("x" -> -1), json) shouldEqual -1.0
  }

  it should "parse and evaluate max" in {
    val json = "{\"type\":\"MAX\",\"lhs\":{\"type\":\"NUM_SYMBOL\",\"key\":\"x\"}," +
      "\"rhs\":{\"type\":\"NUM_LITERAL\",\"value\":100}}"

    Evaluate.fromJsonAsNumber(Map.empty[String, Any], json) shouldEqual 100.0
    Evaluate.fromJsonAsNumber(Map("x" -> 110), json) shouldEqual 110.0
    Evaluate.fromJsonAsNumber(Map("x" -> -1), json) shouldEqual 100.0
  }
}
