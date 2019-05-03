package org.saswata.expressions.parser

import org.saswata.expressions.Expression.Exp

object LispParser {

  def tokenise(str: String): Seq[String] =
    str
      .replace("(", " ( ")
      .replace(")", " ) ")
      .split("""\s+""")
      .filter(_.length > 0)

  @SuppressWarnings(
    Array("org.wartremover.warts.Throw", "org.wartremover.warts.Any", "org.wartremover.warts.Var")
  )
  def grouped(tokens: Seq[String]): Seq[Any] = {
    if (tokens.isEmpty) throw new IllegalArgumentException("Abrupt End in expression")

    def tillOpenParen(it: Any): Boolean = it match {
      case "(" => false
      case _   => true
    }

    val groupedTokens =
      tokens.foldLeft(List.empty[Any]) { (stack, token) =>
        token match {
          case ")" =>
            val (matched, rest) =
              stack.span(tillOpenParen)
            matched.reverse :: rest.drop(1)
          case s: String => s :: stack
        }
      }

    if (groupedTokens.length != 1) {
      throw new IllegalArgumentException(s"Unbalanced expression $tokens")
    }
    groupedTokens.headOption.fold(Seq.empty[Any]) {
      case exp: Seq[_] => exp
      case unknown     => throw new IllegalArgumentException(s"Illegal expression structure $unknown")
    }
  }

  def parse(input: String): Exp[_] = {
    val groupedOperations = grouped(tokenise(input))

    ???
  }
}
