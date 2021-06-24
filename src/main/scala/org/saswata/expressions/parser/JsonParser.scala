package org.saswata.expressions.parser

import org.json4s.JsonAST.JNumber
import org.json4s.jackson.JsonMethods.parse
import org.json4s.{JArray, JObject, JString, JValue}
import org.saswata.expressions.Expression._

object JsonParser {
  // json schema of input

  //  {
  //    "type": "???",
  //    "key": "???",
  //    "value": "???",
  //    "cond": {???},
  //    "lhs": {???},
  //    "rhs": {???}
  //  }

  def parseJsonObj(jsonStr: String): JObject = asJObject(parse(jsonStr))

  def extractType(json: JObject): String = asString(json \ "type")

  def extractValue(json: JObject): JValue = json \ "value"

  def extractKey(json: JObject): String = asString(json \ "key")

  def extractLhs(json: JObject): JObject = asJObject(json \ "lhs")

  def extractRhs(json: JObject): JObject = asJObject(json \ "rhs")

  def extractCond(json: JObject): JObject = asJObject(json \ "cond")

  val asString: PartialFunction[JValue, String] = { case jStr: JString => jStr.values }

  val asJObject: PartialFunction[JValue, JObject] = { case obj: JObject => obj }

  val asJArray: PartialFunction[JValue, JArray] = { case arr: JArray => arr }

  def asNumbe(json: JValue): Double = json.values match { case n: Number => n.doubleValue() }

  def asNumber: PartialFunction[JValue, Double] = {
    case n: JNumber => n.values match { case n: Number => n.doubleValue() }
  }

  @SuppressWarnings(
    Array("org.wartremover.warts.Throw", "org.wartremover.contrib.warts.ExposedTuples")
  )
  def parseOperator(json: JObject): (String, OperatorName.Value) = {
    val tag = extractType(json)
    val operatorType =
      OperatorName
        .typeOf(tag)
        .getOrElse(throw new IllegalArgumentException(s"Unknown operator type $tag"))

    (tag, operatorType)
  }

  def parseBoolExp(jsonStr: String): Exp[Boolean] =
    parseBoolExp(parseJsonObj(jsonStr))

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def parseBoolExp(json: JObject): Exp[Boolean] = {
    val (tag, operatorType) = parseOperator(json)

    operatorType match {
      case OperatorName.BoolAtoms      => parseBoolAtom(json, tag)
      case OperatorName.UnaryLogicOps  => parseUniBoolOperator(json, tag)
      case OperatorName.BinaryLogicOps => parseBinBoolOperator(json, tag)
      case OperatorName.NaryLogicOps   => parseNaryBoolOperator(json, tag)
      case OperatorName.StrRelationOps => parseBinStrBoolOperator(json, tag)
      case OperatorName.NumRelationOps => parseBinNumBoolOperator(json, tag)
      case OperatorName.StrSetBoolOps  => parseStrSetBoolOperator(json, tag)
      case _                           => throw new IllegalArgumentException(s"Incompatible Boolean operator $tag")
    }
  }

  def parseBoolAtom(json: JObject, typeTag: String): Exp[Boolean] =
    typeTag match {
      case "BOOL_SYMBOL" => BOOL_SYMBOL(extractKey(json))
    }

  def parseUniBoolOperator(json: JObject, typeTag: String): Exp[Boolean] =
    typeTag match {
      case "NOT" => NOT(parseBoolExp(extractRhs(json)))
    }

  def parseNaryBoolOperator(json: JObject, typeTag: String): Exp[Boolean] = {
    val rhs = parseBoolExpArray(asJArray(json \ "rhs"))
    typeTag match {
      case "NARY_AND" => NARY_AND(rhs)
      case "NARY_OR"  => NARY_OR(rhs)
    }
  }

  def parseBoolExpArray(json: JArray): Seq[Exp[Boolean]] =
    json.children.collect { case obj: JObject => parseBoolExp(obj) }

  def parseBinBoolOperator(json: JObject, typeTag: String): Exp[Boolean] = {
    val lhs = extractLhs(json)
    val rhs = extractRhs(json)
    typeTag match {
      case "AND" => AND(parseBoolExp(lhs), parseBoolExp(rhs))
      case "OR"  => OR(parseBoolExp(lhs), parseBoolExp(rhs))
    }
  }

  def parseBinStrBoolOperator(json: JObject, typeTag: String): Exp[Boolean] = {
    val lhs = extractLhs(json)
    val rhs = extractRhs(json)
    typeTag match {
      case "STR_EQUALS"     => STR_EQUALS(parseStrAtom(lhs), parseStrAtom(rhs))
      case "STR_NOT_EQUALS" => STR_NOT_EQUALS(parseStrAtom(lhs), parseStrAtom(rhs))
    }
  }

  def parseBinNumBoolOperator(json: JObject, typeTag: String): Exp[Boolean] = {
    val lhs = extractLhs(json)
    val rhs = extractRhs(json)
    typeTag match {
      case "EQUALS"          => EQUALS(parseNumExp(lhs), parseNumExp(rhs))
      case "NOT_EQUALS"      => NOT_EQUALS(parseNumExp(lhs), parseNumExp(rhs))
      case "LESSER_THAN"     => LESSER_THAN(parseNumExp(lhs), parseNumExp(rhs))
      case "LESSER_THAN_EQ"  => LESSER_THAN_EQ(parseNumExp(lhs), parseNumExp(rhs))
      case "GREATER_THAN"    => GREATER_THAN(parseNumExp(lhs), parseNumExp(rhs))
      case "GREATER_THAN_EQ" => GREATER_THAN_EQ(parseNumExp(lhs), parseNumExp(rhs))
    }
  }

  def parseStrSetBoolOperator(json: JObject, typeTag: String): Exp[Boolean] = {
    val lhs = extractLhs(json)
    val rhs = extractRhs(json)
    typeTag match {
      case "STR_SET_CONTAINS" => STR_SET_CONTAINS(parseStrSetAtom(lhs), parseStrAtom(rhs))
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def parseStrSetAtom(json: JObject): Exp[Set[String]] = {
    val (tag, operatorType) = parseOperator(json)

    operatorType match {
      case OperatorName.StrSetAtoms =>
        tag match {
          case "STR_SET_SYMBOL" => STR_SET_SYMBOL(extractKey(json))
        }
      case _ => throw new IllegalArgumentException(s"Incompatible SET atom type $tag")
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def parseStrAtom(json: JObject): Exp[String] = {
    val (tag, operatorType) = parseOperator(json)

    operatorType match {
      case OperatorName.StrAtoms =>
        tag match {
          case "STR_LITERAL" => STR_LITERAL(asString(extractValue(json)))
          case "STR_SYMBOL"  => STR_SYMBOL(extractKey(json))
        }
      case _ => throw new IllegalArgumentException(s"Incompatible String atom type $tag")
    }
  }

  def parseNumExp(jsonStr: String): Exp[Double] =
    parseNumExp(parseJsonObj(jsonStr))

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def parseNumExp(json: JObject): Exp[Double] = {
    val (tag, operatorType) = parseOperator(json)

    operatorType match {
      case OperatorName.NumAtoms            => parseNumAtom(json, tag)
      case OperatorName.UnaryArithmeticOps  => parseUnaryNumOperator(json, tag)
      case OperatorName.BinaryArithmeticOps => parseBinNumOperator(json, tag)
      case OperatorName.If                  => parseIfCondition(json)
      case _                                => throw new IllegalArgumentException(s"Incompatible Numeric operator $tag")
    }
  }

  def parseNumAtom(json: JObject, typeTag: String): Exp[Double] =
    typeTag match {
      case "NUM_LITERAL" =>
        val value: Double = asNumber(extractValue(json))
        NUM_LITERAL(value)
      case "NUM_SYMBOL" => NUM_SYMBOL(extractKey(json))
    }

  def parseUnaryNumOperator(json: JObject, typeTag: String): Exp[Double] = {
    val rhs = extractRhs(json)
    typeTag match {
      case "NEGATE" => NEGATE(parseNumExp(rhs))
    }
  }

  def parseBinNumOperator(json: JObject, typeTag: String): Exp[Double] = {
    val lhs = extractLhs(json)
    val rhs = extractRhs(json)
    typeTag match {
      case "ADD"      => ADD(parseNumExp(lhs), parseNumExp(rhs))
      case "SUBTRACT" => SUBTRACT(parseNumExp(lhs), parseNumExp(rhs))
      case "MULTIPLY" => MULTIPLY(parseNumExp(lhs), parseNumExp(rhs))
      case "DIVIDE"   => DIVIDE(parseNumExp(lhs), parseNumExp(rhs))
      case "MIN"      => MIN(parseNumExp(lhs), parseNumExp(rhs))
      case "MAX"      => MAX(parseNumExp(lhs), parseNumExp(rhs))
    }
  }

  def parseIfCondition(json: JObject): Exp[Double] = {
    val lhs = extractLhs(json)
    val rhs = extractRhs(json)
    val cond = extractCond(json)
    IF(parseBoolExp(cond), parseNumExp(lhs), parseNumExp(rhs))
  }

}
