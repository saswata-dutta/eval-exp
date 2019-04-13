package org.saswata.expressions

import org.json4s._
import org.json4s.jackson.JsonMethods.parse
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

  def parseJsonObj(jsonStr: String): JObject = parse(jsonStr).asInstanceOf[JObject]

  def extractType(json: JObject): String = (json \ "type").asInstanceOf[JString].values

  def extractValue(json: JObject): JValue = json \ "value"

  def extractKey(json: JObject): String = (json \ "key").asInstanceOf[JString].values

  def extractLhs(json: JObject): JObject = (json \ "lhs").asInstanceOf[JObject]

  def extractRhs(json: JObject): JObject = (json \ "rhs").asInstanceOf[JObject]

  def extractCond(json: JObject): JObject = (json \ "cond").asInstanceOf[JObject]

  def parseOperatorType(json: JObject): (String, OperatorType.Value) = {
    val tag = extractType(json)
    val operatorType = OperatorType
      .typeOf(tag)
      .getOrElse(throw new IllegalArgumentException(s"Unknown operator type $tag"))

    (tag, operatorType)
  }

  def parseBoolExp(jsonStr: String): Exp[Boolean] =
    parseBoolExp(parseJsonObj(jsonStr))

  def parseBoolExp(json: JObject): Exp[Boolean] = {
    val (tag, operatorType) = parseOperatorType(json)

    operatorType match {
      case OperatorType.BoolAtoms      => parseBoolAtom(json, tag)
      case OperatorType.UnaryLogicOps  => parseUniBoolOperator(json, tag)
      case OperatorType.BinaryLogicOps => parseBinBoolOperator(json, tag)
      case OperatorType.NaryLogicOps   => parseNaryBoolOperator(json, tag)
      case OperatorType.StrRelationOps => parseBinStrBoolOperator(json, tag)
      case OperatorType.NumRelationOps => parseBinNumBoolOperator(json, tag)
      case OperatorType.StrSetBoolOps  => parseStrSetBoolOperator(json, tag)
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
    val rhs = parseBoolExpArray((json \ "rhs").asInstanceOf[JArray])
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

  def parseStrSetAtom(json: JObject): Exp[Set[String]] = {
    val (tag, operatorType) = parseOperatorType(json)

    operatorType match {
      case OperatorType.StrSetAtoms =>
        tag match {
          case "STR_SET_SYMBOL" => STR_SET_SYMBOL(extractKey(json))
        }
      case _ => throw new IllegalArgumentException(s"Incompatible SET atom type $tag")
    }
  }

  def parseStrAtom(json: JObject): Exp[String] = {
    val (tag, operatorType) = parseOperatorType(json)

    operatorType match {
      case OperatorType.StrAtoms =>
        tag match {
          case "STR_LITERAL" => STR_LITERAL(extractValue(json).asInstanceOf[JString].values)
          case "STR_SYMBOL"  => STR_SYMBOL(extractKey(json))
        }
      case _ => throw new IllegalArgumentException(s"Incompatible String atom type $tag")
    }
  }

  def parseNumExp(jsonStr: String): Exp[Double] =
    parseNumExp(parseJsonObj(jsonStr))

  def parseNumExp(json: JObject): Exp[Double] = {
    val (tag, operatorType) = parseOperatorType(json)

    operatorType match {
      case OperatorType.NumAtoms            => parseNumAtom(json, tag)
      case OperatorType.BinaryArithmeticOps => parseBinNumOperator(json, tag)
      case OperatorType.If                  => parseIfCondition(json)
      case _                                => throw new IllegalArgumentException(s"Incompatible Numeric operator $tag")
    }
  }

  def parseNumAtom(json: JObject, typeTag: String): Exp[Double] =
    typeTag match {
      case "NUM_LITERAL" =>
        val value: Double = extractValue(json).values.asInstanceOf[Number].doubleValue()
        NUM_LITERAL(value)
      case "NUM_SYMBOL" => NUM_SYMBOL(extractKey(json))
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
