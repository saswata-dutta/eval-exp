package org.saswata.expressions.parser

import org.saswata.expressions.Expression.Exp

object LispParser {

  def tokenise(str: String): Seq[String] =
    str.replace("(", " ( ").replace(")", " ) ").split("""\s+""").filter(_.length > 0)

  def grouped(tokens: Seq[String]): Seq[Any] = {
    if (tokens.isEmpty) throw new IllegalArgumentException("Abrupt End in expression")

    var stack = List.empty[Any]

    tokens.foreach {
      case ")" =>
        val (matched, rest) =
          stack.span(it => !it.isInstanceOf[String] || it.asInstanceOf[String] != "(")
        stack = matched.reverse :: rest.tail

      case s: String => stack = s :: stack
    }

    if (stack.length != 1) throw new IllegalArgumentException(s"Unbalanced expression $tokens")
    stack.head match {
      case exp: Seq[_] => exp
      case unknown     => throw new IllegalArgumentException(s"Illegal expression structure $unknown")
    }
  }

  def parseBoolExp(input: String): Exp[Boolean] = {
    val groupedOperations = grouped(tokenise(input))

    ???
  }

  def parseNumExp(input: String): Exp[Double] = {
    val groupedOperations = grouped(tokenise(input))

    ???
  }

  def parse(input: String): Exp[_] = {
    val groupedOperations = grouped(tokenise(input))

    ???
  }
}
