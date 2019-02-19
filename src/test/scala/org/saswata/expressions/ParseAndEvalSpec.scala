package org.saswata.expressions

import org.saswata.expressions.Expression._
import org.scalatest._

class ParseAndEvalSpec extends FlatSpec with Matchers {

  "The Parser and Evaluator" should "handle atomic ints" in {
    val json =
      """
        |{
        |  "type": "NUM_LITERAL",
        |  "value": 1
        |}
      """.stripMargin

    val exp = JsonParser.parseNumExp(json)
    val result: Double = new Expression().eval[Double](exp)
    result shouldEqual 1.0
  }

  it should "handle atomic decimals with 1 significant digit" in {
    val json =
      """
        |{
        |  "type": "NUM_LITERAL",
        |  "value": 100.1
        |}
      """.stripMargin

    val exp = JsonParser.parseNumExp(json)
    val result: Double = new Expression().eval[Double](exp)
    result shouldEqual 100.1
  }

  it should "handle atomic decimals with 2 significant digits" in {
    val json =
      """
        |{
        |  "type": "NUM_LITERAL",
        |  "value": 100.12
        |}
      """.stripMargin

    val exp = JsonParser.parseNumExp(json)
    val result: Double = new Expression().eval[Double](exp)
    result shouldEqual 100.12
  }

  it should "handle numeric symbols" in {
    val json =
      """
        |{
        |  "type": "NUM_SYMBOL",
        |  "key": "num"
        |}
      """.stripMargin

    val exp = JsonParser.parseNumExp(json)
    new Expression().eval[Double](exp) shouldEqual 0.0
    new Expression(Map("num" -> 1)).eval[Double](exp) shouldEqual 1.0
    new Expression(Map("num" -> 100L)).eval[Double](exp) shouldEqual 100.0
    new Expression(Map("num" -> 10.5)).eval[Double](exp) shouldEqual 10.5
    new Expression(Map("num" -> 10.5F)).eval[Double](exp) shouldEqual 10.5
    new Expression(Map("num" -> BigDecimal(100.5))).eval[Double](exp) shouldEqual 100.5
    new Expression(Map("num" -> BigInt(100))).eval[Double](exp) shouldEqual 100.0
  }

  it should "handle bool symbols" in {
    val json =
      """
        |{
        |  "type": "BOOL_SYMBOL",
        |  "key": "flag"
        |}
      """.stripMargin

    val exp = JsonParser.parseBoolExp(json)
    new Expression().eval[Boolean](exp) shouldEqual false
    new Expression(Map("num" -> 1)).eval[Boolean](exp) shouldEqual false
    new Expression(Map("flag" -> false)).eval[Boolean](exp) shouldEqual false
    new Expression(Map("flag" -> true)).eval[Boolean](exp) shouldEqual true
  }

  it should "handle numeric add" in {
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
    val result: Double = new Expression().eval[Double](exp)
    result shouldEqual 2.1
  }

  it should "handle numeric subtract" in {
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
    val result: Double = new Expression().eval[Double](exp)
    result shouldEqual 0.25
  }

  it should "handle numeric multiply" in {
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
    val result: Double = new Expression().eval[Double](exp)
    result shouldEqual 2.5
  }

  it should "handle numeric divide" in {
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
    val result: Double = new Expression().eval[Double](exp)
    result shouldEqual (5.5 / 1.25)
  }

  it should "handle numeric equals" in {
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
    val result: Boolean = new Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "handle numeric not equals" in {
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
    val result: Boolean = new Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "handle numeric lesser equals for equal" in {
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
    val result: Boolean = new Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "handle numeric lesser equals" in {
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
    val result: Boolean = new Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "handle numeric lesser" in {
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
    val result: Boolean = new Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "handle numeric greater equals for equals" in {
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
    val result: Boolean = new Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "handle numeric greater equals" in {
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
    val result: Boolean = new Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "handle numeric greater" in {
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
    val result: Boolean = new Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "handle atomic strings" in {
    val json =
      """
        |{
        |  "type": "STR_LITERAL",
        |  "value": "HELLO"
        |}
      """.stripMargin

    val exp = JsonParser.parseStrAtom(JsonParser.parseJsonObj(json))
    val result: String = new Expression().eval[String](exp)
    result shouldEqual "HELLO"
  }

  it should "handle string equals" in {
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
    val result: Boolean = new Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "handle string not equals" in {
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
    val result: Boolean = new Expression().eval[Boolean](exp)
    result shouldEqual true
  }

  it should "handle string symbol" in {
    val json =
      """
        |{
        |  "type": "STR_SYMBOL",
        |  "key": "x"
        |}
      """.stripMargin

    val exp = JsonParser.parseStrAtom(JsonParser.parseJsonObj(json))
    val result: String = new Expression(Map("x" -> "HELLO")).eval[String](exp)
    result shouldEqual "HELLO"
  }

  it should "handle num symbol" in {
    val json =
      """
        |{
        |  "type": "NUM_SYMBOL",
        |  "key": "x"
        |}
      """.stripMargin

    val exp = JsonParser.parseNumExp(json)
    val result: Double = new Expression(Map("x" -> 6.5)).eval[Double](exp)
    result shouldEqual 6.5
  }

  it should "handle and operation" in {
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
    val result: Boolean = new Expression(Map("str" -> "Hi", "num" -> 1.1)).eval[Boolean](exp)
    result shouldEqual true
  }

  it should "handle or operation" in {
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
    val result: Boolean = new Expression(Map("str" -> "Hi", "num" -> 1.2)).eval[Boolean](exp)
    result shouldEqual true
  }

  it should "handle not operation" in {
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
    val result: Boolean = new Expression(Map("str" -> "Hey", "num" -> 1.2)).eval[Boolean](exp)
    result shouldEqual true
  }

  it should "handle if condition using bool op" in {
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

    val result1: Double = new Expression(Map("str" -> "Hi", "num" -> 10.1)).eval[Double](exp)
    result1 shouldEqual 1.1

    val result2: Double = new Expression(Map("str" -> "Ho", "num" -> 10.1)).eval[Double](exp)
    result2 shouldEqual 10.1
  }

  it should "handle if condition using bool symbol" in {
    val json =
      """
        |{
        |    "cond": {
        |        "key": "flag",
        |        "type": "BOOL_SYMBOL"
        |    },
        |    "lhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 10
        |    },
        |    "rhs": {
        |        "type": "NUM_LITERAL",
        |        "value": 15
        |    },
        |    "type": "IF"
        |}
      """.stripMargin

    val exp = JsonParser.parseNumExp(json)

    new Expression().eval[Double](exp) shouldEqual 15.0
    new Expression(Map("flag" -> false)).eval[Double](exp) shouldEqual 15.0
    new Expression(Map("flag" -> true)).eval[Double](exp) shouldEqual 10.0
  }

  it should "handle explicit expression class constructors" in {
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
        |        "key": "num1",
        |        "type": "NUM_SYMBOL"
        |    },
        |    "rhs": {
        |        "key": "num2",
        |        "type": "NUM_SYMBOL"
        |    },
        |    "type": "IF"
        |}
      """.stripMargin

    val exp = JsonParser.parseNumExp(json)

    val result1: Double = new Expression(Map("str" -> "Hi", "num1" -> 10)).eval[Double](exp)
    result1 shouldEqual 10.0

    val jmap: java.util.Map[java.lang.String, java.lang.Object] =
      new java.util.HashMap[java.lang.String, java.lang.Object]()
    jmap.put("str", "Ho")
    jmap.put("num2", java.lang.Integer.valueOf(100))

    val result2: Double = new Expression(jmap).eval[Double](exp)
    result2 shouldEqual 100.0

    val result3: Double = new Expression().eval[Double](exp)
    result3 shouldEqual 0.0
  }

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
