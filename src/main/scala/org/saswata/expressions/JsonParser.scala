package org.saswata.expressions

import org.saswata.expressions.Expression._

object JsonParser {

  import org.json4s._
  import org.json4s.jackson.JsonMethods.parse

  // json schema of input

  //  {
  //    "type": "???",
  //    "key": "???",
  //    "value": "???" / ???,
  //    "lhs": {},
  //    "rhs": {}
  //  }

  private val binary_str_bool_ops: Seq[String] = Seq("STR_EQUALS", "STR_NOT_EQUALS")
  private val binary_num_bool_ops: Seq[String] = Seq("EQUALS", "NOT_EQUALS",
    "LESSER_THAN", "LESSER_THAN_EQ", "GREATER_THAN", "GREATER_THAN_EQ")
  private val binary_bool_ops: Seq[String] = Seq("AND", "OR")
  private val unary_bool_ops: Seq[String] = Seq("NOT")

  private val binary_num_ops: Seq[String] = Seq("ADD", "SUBTRACT", "MULTIPLY", "DIVIDE")

  private val str_atom: Seq[String] = Seq("STR_SYMBOL", "STR_LITERAL")
  private val num_atom: Seq[String] = Seq("NUM_SYMBOL", "NUM_LITERAL")

  def parseJsonObj(jsonStr: String): JObject = parse(jsonStr).asInstanceOf[JObject]

  def extractType(json: JObject): String = (json \ "type").asInstanceOf[JString].values

  def extractValue(json: JObject): JValue = json \ "value"

  def extractKey(json: JObject): String = (json \ "key").asInstanceOf[JString].values

  def extractLhs(json: JObject): JObject = (json \ "lhs").asInstanceOf[JObject]

  def extractRhs(json: JObject): JObject = (json \ "rhs").asInstanceOf[JObject]

  def parseBoolExp(jsonStr: String): Exp[Boolean] = {
    parseBoolExp(parseJsonObj(jsonStr))
  }

  def parseBoolExp(json: JObject): Exp[Boolean] = {
    extractType(json) match {
      case tag if unary_bool_ops.contains(tag) => parseUniBoolOperator(json, tag)
      case tag if binary_bool_ops.contains(tag) => parseBinBoolOperator(json, tag)
      case tag if binary_str_bool_ops.contains(tag) => parseBinStrBoolOperator(json, tag)
      case tag if binary_num_bool_ops.contains(tag) => parseBinNumBoolOperator(json, tag)
    }
  }

  def parseUniBoolOperator(json: JObject, typeTag: String): Exp[Boolean] = {
    typeTag match {
      case "NOT" => NOT(parseBoolExp(extractRhs(json)))
    }
  }

  def parseBinBoolOperator(json: JObject, typeTag: String): Exp[Boolean] = {
    val lhs = extractLhs(json)
    val rhs = extractRhs(json)
    typeTag match {
      case "AND" => AND(parseBoolExp(lhs), parseBoolExp(rhs))
      case "OR" => OR(parseBoolExp(lhs), parseBoolExp(rhs))
    }
  }

  def parseBinStrBoolOperator(json: JObject, typeTag: String): Exp[Boolean] = {
    val lhs = extractLhs(json)
    val rhs = extractRhs(json)
    typeTag match {
      case "STR_EQUALS" => STR_EQUALS(parseStrAtom(lhs), parseStrAtom(rhs))
      case "STR_NOT_EQUALS" => STR_NOT_EQUALS(parseStrAtom(lhs), parseStrAtom(rhs))
    }
  }

  def parseBinNumBoolOperator(json: JObject, typeTag: String): Exp[Boolean] = {
    val lhs = extractLhs(json)
    val rhs = extractRhs(json)
    typeTag match {
      case "EQUALS" => EQUALS(parseNumExp(lhs), parseNumExp(rhs))
      case "NOT_EQUALS" => NOT_EQUALS(parseNumExp(lhs), parseNumExp(rhs))
      case "LESSER_THAN" => LESSER_THAN(parseNumExp(lhs), parseNumExp(rhs))
      case "LESSER_THAN_EQ" => LESSER_THAN_EQ(parseNumExp(lhs), parseNumExp(rhs))
      case "GREATER_THAN" => GREATER_THAN(parseNumExp(lhs), parseNumExp(rhs))
      case "GREATER_THAN_EQ" => GREATER_THAN_EQ(parseNumExp(lhs), parseNumExp(rhs))
    }
  }

  def parseStrAtom(json: JObject): Exp[String] = {
    extractType(json) match {
      case tag if str_atom.contains(tag) => tag match {
        case "STR_LITERAL" => STR_LITERAL(extractValue(json).asInstanceOf[JString].values)
        case "STR_SYMBOL" => STR_SYMBOL(extractKey(json))
      }
    }
  }

  def parseNumExp(jsonStr: String): Exp[Double] = {
    parseNumExp(parseJsonObj(jsonStr))
  }

  def parseNumExp(json: JObject): Exp[Double] = {
    extractType(json) match {
      case tag if num_atom.contains(tag) => parseNumAtom(json, tag)
      case tag if binary_num_ops.contains(tag) => parseBinNumOperator(json, tag)
    }
  }

  def parseNumAtom(json: JObject, typeTag: String): Exp[Double] = {
    typeTag match {
      case "NUM_LITERAL" =>
        val value: Double = extractValue(json) match {
          case JInt(num) => num.toDouble
          case JLong(num) => num.toDouble
          case JDouble(num) => num
          case JDecimal(num) => num.toDouble
        }
        NUM_LITERAL(value)

      case "NUM_SYMBOL" => NUM_SYMBOL(extractKey(json))
    }
  }

  def parseBinNumOperator(json: JObject, typeTag: String): Exp[Double] = {
    val lhs = extractLhs(json)
    val rhs = extractRhs(json)
    typeTag match {
      case "ADD" => ADD(parseNumExp(lhs), parseNumExp(rhs))
      case "SUBTRACT" => SUBTRACT(parseNumExp(lhs), parseNumExp(rhs))
      case "MULTIPLY" => MULTIPLY(parseNumExp(lhs), parseNumExp(rhs))
      case "DIVIDE" => DIVIDE(parseNumExp(lhs), parseNumExp(rhs))
    }
  }

}
