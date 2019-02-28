package org.saswata.expressions

object OperatorType extends Enumeration {

  protected case class Val(operations: Seq[String]) extends super.Val

  implicit def valueToExpressionTypesVal(x: Value): Val = x.asInstanceOf[Val]

  val BINARY_STR_BOOL_OPS = Val(Seq("STR_EQUALS", "STR_NOT_EQUALS"))
  val BINARY_NUM_BOOL_OPS = Val(Seq("EQUALS", "NOT_EQUALS",
    "LESSER_THAN", "LESSER_THAN_EQ", "GREATER_THAN", "GREATER_THAN_EQ"))
  val NARY_BOOL_OPS = Val(Seq("NARY_AND", "NARY_OR"))
  val BINARY_BOOL_OPS = Val(Seq("AND", "OR"))
  val UNARY_BOOL_OPS = Val(Seq("NOT"))
  val BINARY_NUM_OPS = Val(Seq("ADD", "SUBTRACT", "MULTIPLY", "DIVIDE"))
  val STR_ATOMS = Val(Seq("STR_SYMBOL", "STR_LITERAL"))
  val NUM_ATOMS = Val(Seq("NUM_SYMBOL", "NUM_LITERAL"))
  val BOOL_ATOMS = Val(Seq("BOOL_SYMBOL"))
  val IF = Val(Seq("IF"))

  def typeOf(operation: String): Option[OperatorType.Value] = {
    OperatorType.values.find(_.operations.contains(operation))
  }
}
