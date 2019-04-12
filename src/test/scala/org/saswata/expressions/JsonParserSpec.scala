package org.saswata.expressions

import org.json4s.JsonAST.JObject
import org.scalatest.{FlatSpec, Matchers}

class JsonParserSpec extends FlatSpec with Matchers {
  "The Json Parser" should "indicate error in case of unknown json 'type' tag" in {
    val jsonStr =
      """
        |{
        |  "type": "NUMBER",
        |  "value": 100.1
        |}
      """.stripMargin
    val json: JObject = JsonParser.parseJsonObj(jsonStr)

    def assertParserThrows(parser: JObject => Any): Boolean = {
      val thrown = intercept[IllegalArgumentException](parser(json))
      thrown.getMessage == "Unknown operator type NUMBER"
    }

    assertParserThrows(JsonParser.parseOperatorType) shouldEqual true
    assertParserThrows(JsonParser.parseNumExp) shouldEqual true
    assertParserThrows(JsonParser.parseBoolExp) shouldEqual true
  }

  it should "indicate error in case of incompatible json 'type' tag for numeric expression" in {
    val json =
      """
        |{
        |  "type": "BOOL_SYMBOL",
        |  "key": "flag"
        |}
      """.stripMargin

    val thrown = intercept[IllegalArgumentException](JsonParser.parseNumExp(json))
    thrown.getMessage shouldEqual "Incompatible Numeric operator BOOL_SYMBOL"
  }

  it should "indicate error in case of incompatible json 'type' tag for boolean expression" in {
    val json =
      """
        |{
        |  "type": "NUM_LITERAL",
        |  "value": 100.1
        |}
      """.stripMargin

    val thrown = intercept[IllegalArgumentException](JsonParser.parseBoolExp(json))
    thrown.getMessage shouldEqual "Incompatible Boolean operator NUM_LITERAL"
  }

  it should "indicate error in case of IF not conforming to numeric type" in {
    val json =
      """
        |{
        |    "cond": {
        |        "key": "flag",
        |        "type": "BOOL_SYMBOL"
        |    },
        |    "lhs": {
        |        "type": "BOOL_SYMBOL",
        |        "key": "foo"
        |    },
        |    "rhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 15
        |    },
        |    "type": "IF"
        |}
      """.stripMargin

    val thrown = intercept[IllegalArgumentException](JsonParser.parseNumExp(json))
    thrown.getMessage shouldEqual "Incompatible Numeric operator BOOL_SYMBOL"
  }

  it should "indicate error in case Numeric type is compared as String" in {
    val json =
      """
        |{
        |    "type": "STR_EQUALS",
        |    "lhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 100.1
        |    },
        |    "rhs": {
        |        "type": "STR_LITERAL",
        |        "value": "Hi"
        |    }
        |}
      """.stripMargin

    val thrown = intercept[IllegalArgumentException](JsonParser.parseBoolExp(json))
    thrown.getMessage shouldEqual "Incompatible String atom type NUM_LITERAL"
  }

  it should "indicate error in case String type is compared as Number" in {
    val json =
      """
        |{
        |    "type": "EQUALS",
        |    "lhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 100.1
        |    },
        |    "rhs": {
        |        "type": "STR_LITERAL",
        |        "value": "Hi"
        |    }
        |}
      """.stripMargin

    val thrown = intercept[IllegalArgumentException](JsonParser.parseBoolExp(json))
    thrown.getMessage shouldEqual "Incompatible Numeric operator STR_LITERAL"
  }

  it should "indicate error in case String type is compared as SET" in {
    val json =
      """
        |{
        |    "lhs": {
        |        "key": "tags",
        |        "type": "STR_SYMBOL"
        |    },
        |    "rhs": {
        |        "key": "tag",
        |        "type": "STR_SYMBOL"
        |    },
        |    "type": "STR_SET_CONTAINS"
        |}
      """.stripMargin

    val thrown = intercept[IllegalArgumentException](JsonParser.parseBoolExp(json))
    thrown.getMessage shouldEqual "Incompatible SET atom type STR_SYMBOL"
  }

  it should "handle negation of list contains" in {
    val json = "{\"type\":\"NOT\",\"rhs\":{\"type\":\"STR_SET_CONTAINS\",\"lhs\":" +
      "{\"type\":\"STR_SET_SYMBOL\",\"key\":\"supply_user_tag\"},\"rhs\":" +
      "{\"type\":\"STR_SYMBOL\",\"key\":\"pas\"}}}"

    val exp = JsonParser.parseBoolExp(json)

    val result1: Boolean = new Expression(Map("supply_user_tag" -> Seq("a", "b", "c"), "pas" -> "c")).eval[Boolean](exp)
    result1 shouldEqual false

    val result2: Boolean = new Expression(Map("supply_user_tag" -> Seq("a", "b", "c"), "pas" -> "d")).eval[Boolean](exp)
    result2 shouldEqual true
  }
}
