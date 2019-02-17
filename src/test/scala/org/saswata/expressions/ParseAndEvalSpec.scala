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
    val result: Double = Expression().eval[Double](exp)
    result shouldEqual 1.0
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
    val result: Double = Expression().eval[Double](exp)
    result shouldEqual 100.1
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
    val result: Double = Expression().eval[Double](exp)
    result shouldEqual 100.12
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
    val result: String = Expression().eval[String](exp)
    result shouldEqual "HELLO"
  }

  it should "work for numeric add" in {
    val json =
      """
        |{
        |    "type": "ADD",
        |    "lhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 1.1
        |    },
        |    "rhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 1
        |    }
        |}
      """.stripMargin


    val exp = JsonParser.parseNumExp(json)
    val result: Double = Expression().eval[Double](exp)
    result shouldEqual 2.1
  }

  it should "work for numeric subtract" in {
    val json =
      """
        |{
        |    "type": "SUBTRACT",
        |    "lhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 1.25
        |    },
        |    "rhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 1
        |    }
        |}
      """.stripMargin

    val exp = JsonParser.parseNumExp(json)
    val result: Double = Expression().eval[Double](exp)
    result shouldEqual 0.25
  }

  it should "work for numeric multiply" in {
    val json =
      """
        |{
        |    "type": "MULTIPLY",
        |    "lhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 2
        |    },
        |    "rhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 1.25
        |    }
        |}
      """.stripMargin

    val exp = JsonParser.parseNumExp(json)
    val result: Double = Expression().eval[Double](exp)
    result shouldEqual 2.5
  }

  it should "work for numeric divide" in {
    val json =
      """
        |{
        |    "type": "DIVIDE",
        |    "lhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 5.5
        |    },
        |    "rhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 1.25
        |    }
        |}
      """.stripMargin

    val exp = JsonParser.parseNumExp(json)
    val result: Double = Expression().eval[Double](exp)
    result shouldEqual (5.5 / 1.25)
  }

  it should "work for numeric equals" in {
    val json =
      """
        |{
        |    "type": "EQUALS",
        |    "lhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 5.5
        |    },
        |    "rhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 5.5
        |    }
        |}
      """.stripMargin

    val exp = JsonParser.parseBoolExp(json)
    val result: Boolean = Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "work for numeric not equals" in {
    val json =
      """
        |{
        |    "type": "NOT_EQUALS",
        |    "lhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 5.5
        |    },
        |    "rhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 6.5
        |    }
        |}
      """.stripMargin

    val exp = JsonParser.parseBoolExp(json)
    val result: Boolean = Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "work for numeric lesser equals for equal" in {
    val json =
      """
        |{
        |    "type": "LESSER_THAN_EQ",
        |    "lhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 5.5
        |    },
        |    "rhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 5.5
        |    }
        |}
      """.stripMargin

    val exp = JsonParser.parseBoolExp(json)
    val result: Boolean = Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "work for numeric lesser equals" in {
    val json =
      """
        |{
        |    "type": "LESSER_THAN_EQ",
        |    "lhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 5.5
        |    },
        |    "rhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 6.5
        |    }
        |}
      """.stripMargin

    val exp = JsonParser.parseBoolExp(json)
    val result: Boolean = Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "work for numeric lesser" in {
    val json =
      """
        |{
        |    "type": "LESSER_THAN",
        |    "lhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 5.5
        |    },
        |    "rhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 6.5
        |    }
        |}
      """.stripMargin

    val exp = JsonParser.parseBoolExp(json)
    val result: Boolean = Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "work for numeric greater equals for equals" in {
    val json =
      """
        |{
        |    "type": "GREATER_THAN_EQ",
        |    "lhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 7.5
        |    },
        |    "rhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 7.5
        |    }
        |}
      """.stripMargin

    val exp = JsonParser.parseBoolExp(json)
    val result: Boolean = Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "work for numeric greater equals" in {
    val json =
      """
        |{
        |    "type": "GREATER_THAN_EQ",
        |    "lhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 7.5
        |    },
        |    "rhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 6.5
        |    }
        |}
      """.stripMargin

    val exp = JsonParser.parseBoolExp(json)
    val result: Boolean = Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "work for numeric greater" in {
    val json =
      """
        |{
        |    "type": "GREATER_THAN",
        |    "lhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 7.5
        |    },
        |    "rhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 6.5
        |    }
        |}
      """.stripMargin

    val exp = JsonParser.parseBoolExp(json)
    val result: Boolean = Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "work for string equals" in {
    val json =
      """
        |{
        |    "type": "STR_EQUALS",
        |    "lhs": {
        |        "type": "STR_LITERAL",
        |        "value": "Hi"
        |    },
        |    "rhs": {
        |        "type": "STR_LITERAL",
        |        "value": "Hi"
        |    }
        |}
      """.stripMargin

    val exp = JsonParser.parseBoolExp(json)
    val result: Boolean = Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "work for string not equals" in {
    val json =
      """
        |{
        |    "type": "STR_NOT_EQUALS",
        |    "lhs": {
        |        "type": "STR_LITERAL",
        |        "value": "Hi"
        |    },
        |    "rhs": {
        |        "type": "STR_LITERAL",
        |        "value": "Ho"
        |    }
        |}
      """.stripMargin

    val exp = JsonParser.parseBoolExp(json)
    val result: Boolean = Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "work for string symbol" in {
    val json =
      """
        |{
        |	"type": "STR_SYMBOL",
        |	"key": "x"
        |}
      """.stripMargin

    val exp = JsonParser.parseStrAtom(JsonParser.parseJsonObj(json))
    val result: String = Expression(Map("x" -> "HELLO")).eval[String](exp)
    result shouldEqual "HELLO"
  }

  it should "work for num symbol" in {
    val json =
      """
        |{
        |	"type": "NUM_SYMBOL",
        |	"key": "x"
        |}
      """.stripMargin

    val exp = JsonParser.parseNumExp(json)
    val result: Double = Expression(Map("x" -> 6.5)).eval[Double](exp)
    result shouldEqual 6.5
  }

  it should "work for and operation" in {
    val json =
      """
        |{
        |    "lhs": {
        |        "lhs": {
        |            "type": "NUM_LITERAL",
        |            "value": 1.1
        |        },
        |        "rhs": {
        |            "key": "num",
        |            "type": "NUM_SYMBOL"
        |        },
        |        "type": "EQUALS"
        |    },
        |    "rhs": {
        |        "lhs": {
        |            "type": "STR_LITERAL",
        |            "value": "Hi"
        |        },
        |        "rhs": {
        |            "key": "str",
        |            "type": "STR_SYMBOL"
        |        },
        |        "type": "STR_EQUALS"
        |    },
        |    "type": "AND"
        |}
      """.stripMargin

    val exp = JsonParser.parseBoolExp(json)
    val result: Boolean = Expression(Map("str" -> "Hi", "num" -> 1.1)).eval[Boolean](exp)
    result shouldEqual true
  }

  it should "work for or operation" in {
    val json =
      """
        |{
        |    "lhs": {
        |        "lhs": {
        |            "type": "NUM_LITERAL",
        |            "value": 1.1
        |        },
        |        "rhs": {
        |            "key": "num",
        |            "type": "NUM_SYMBOL"
        |        },
        |        "type": "EQUALS"
        |    },
        |    "rhs": {
        |        "lhs": {
        |            "type": "STR_LITERAL",
        |            "value": "Hi"
        |        },
        |        "rhs": {
        |            "key": "str",
        |            "type": "STR_SYMBOL"
        |        },
        |        "type": "STR_EQUALS"
        |    },
        |    "type": "OR"
        |}
      """.stripMargin

    val exp = JsonParser.parseBoolExp(json)
    val result: Boolean = Expression(Map("str" -> "Hi", "num" -> 1.2)).eval[Boolean](exp)
    result shouldEqual true
  }

  it should "work for not operation" in {
    val json =
      """{
        |    "rhs": {
        |        "lhs": {
        |            "type": "STR_LITERAL",
        |            "value": "Hi"
        |        },
        |        "rhs": {
        |            "key": "str",
        |            "type": "STR_SYMBOL"
        |        },
        |        "type": "STR_EQUALS"
        |    },
        |    "type": "NOT"
        |}
      """.stripMargin

    val exp = JsonParser.parseBoolExp(json)
    val result: Boolean = Expression(Map("str" -> "Hey", "num" -> 1.2)).eval[Boolean](exp)
    result shouldEqual true
  }

  it should "work for if condition" in {
    val json =
      """
        |{
        |    "cond": {
        |        "lhs": {
        |            "type": "STR_LITERAL",
        |            "value": "Hi"
        |        },
        |        "rhs": {
        |            "key": "str",
        |            "type": "STR_SYMBOL"
        |        },
        |        "type": "STR_EQUALS"
        |    },
        |    "lhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 1.1
        |    },
        |    "rhs": {
        |        "key": "num",
        |        "type": "NUM_SYMBOL"
        |    },
        |    "type": "IF"
        |}
      """.stripMargin

    val exp = JsonParser.parseNumExp(json)

    val result1: Double = Expression(Map("str" -> "Hi", "num" -> 10.1)).eval[Double](exp)
    result1 shouldEqual 1.1

    val result2: Double = Expression(Map("str" -> "Ho", "num" -> 10.1)).eval[Double](exp)
    result2 shouldEqual 10.1
  }

}
