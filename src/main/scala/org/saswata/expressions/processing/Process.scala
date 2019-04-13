package org.saswata.expressions.processing

import org.saswata.expressions.Expression._

object Process {

  // scalastyle:off cyclomatic.complexity
  def apply[R](exp: Exp[_], env: Map[String, Any], processor: Processor[R]): R =
    exp match {
      case e: BOOL_SYMBOL      => processor.process(e, env)
      case e: STR_SYMBOL       => processor.process(e, env)
      case e: STR_LITERAL      => processor.process(e, env)
      case e: NUM_SYMBOL       => processor.process(e, env)
      case e: NUM_LITERAL      => processor.process(e, env)
      case e: STR_EQUALS       => processor.process(e, env)
      case e: STR_NOT_EQUALS   => processor.process(e, env)
      case e: EQUALS           => processor.process(e, env)
      case e: NOT_EQUALS       => processor.process(e, env)
      case e: LESSER_THAN      => processor.process(e, env)
      case e: LESSER_THAN_EQ   => processor.process(e, env)
      case e: GREATER_THAN     => processor.process(e, env)
      case e: GREATER_THAN_EQ  => processor.process(e, env)
      case e: NEGATE           => processor.process(e, env)
      case e: AND              => processor.process(e, env)
      case e: OR               => processor.process(e, env)
      case e: NOT              => processor.process(e, env)
      case e: NARY_AND         => processor.process(e, env)
      case e: NARY_OR          => processor.process(e, env)
      case e: ADD              => processor.process(e, env)
      case e: SUBTRACT         => processor.process(e, env)
      case e: MULTIPLY         => processor.process(e, env)
      case e: DIVIDE           => processor.process(e, env)
      case e: IF               => processor.process(e, env)
      case e: STR_SET_SYMBOL   => processor.process(e, env)
      case e: STR_SET_CONTAINS => processor.process(e, env)
    }
  // scalastyle:on cyclomatic.complexity
}
