package org.saswata.expressions.parser

object OperatorName extends Enumeration {

  protected case class Val(operations: Seq[String]) extends super.Val

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  implicit def valueToExpressionTypesVal(x: Value): Val = x.asInstanceOf[Val]

  val StrAtoms = Val(Seq("STR_SYMBOL", "STR_LITERAL"))
  val NumAtoms = Val(Seq("NUM_SYMBOL", "NUM_LITERAL"))
  val BoolAtoms = Val(Seq("BOOL_SYMBOL"))

  val StrRelationOps = Val(Seq("STR_EQUALS", "STR_NOT_EQUALS"))

  val NumRelationOps = Val(
    Seq("EQUALS", "NOT_EQUALS", "LESSER_THAN", "LESSER_THAN_EQ", "GREATER_THAN", "GREATER_THAN_EQ")
  )

  val UnaryLogicOps = Val(Seq("NOT"))
  val BinaryLogicOps = Val(Seq("AND", "OR"))
  val NaryLogicOps = Val(Seq("NARY_AND", "NARY_OR"))

  val StrSetAtoms = Val(Seq("STR_SET_SYMBOL"))
  val StrSetBoolOps = Val(Seq("STR_SET_CONTAINS"))

  val UnaryArithmeticOps = Val(Seq("NEGATE"))
  val BinaryArithmeticOps = Val(Seq("ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "MIN", "MAX"))

  val If = Val(Seq("IF"))

  def typeOf(operation: String): Option[OperatorName.Value] =
    OperatorName.values.find(_.operations.contains(operation))
}
