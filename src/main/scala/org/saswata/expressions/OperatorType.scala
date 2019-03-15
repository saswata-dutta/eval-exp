package org.saswata.expressions

object OperatorType extends Enumeration {

  protected case class Val(operations: Seq[String]) extends super.Val

  implicit def valueToExpressionTypesVal(x: Value): Val = x.asInstanceOf[Val]

  val StrAtoms = Val(Seq("STR_SYMBOL", "STR_LITERAL"))
  val NumAtoms = Val(Seq("NUM_SYMBOL", "NUM_LITERAL"))
  val BoolAtoms = Val(Seq("BOOL_SYMBOL"))

  val StrRelationOps = Val(Seq("STR_EQUALS", "STR_NOT_EQUALS"))
  val NumRelationOps = Val(Seq("EQUALS", "NOT_EQUALS",
    "LESSER_THAN", "LESSER_THAN_EQ", "GREATER_THAN", "GREATER_THAN_EQ"))

  val UnaryLogicOps = Val(Seq("NOT"))
  val BinaryLogicOps = Val(Seq("AND", "OR"))
  val NaryLogicOps = Val(Seq("NARY_AND", "NARY_OR"))

  val StrSetAtom = Val(Seq("STR_SET_SYMBOL"))
  val StrSetOps = Val(Seq("STR_SET_CONTAINS"))

  val BinaryArithmeticOps = Val(Seq("ADD", "SUBTRACT", "MULTIPLY", "DIVIDE"))

  val If = Val(Seq("IF"))

  def typeOf(operation: String): Option[OperatorType.Value] = {
    OperatorType.values.find(_.operations.contains(operation))
  }
}
