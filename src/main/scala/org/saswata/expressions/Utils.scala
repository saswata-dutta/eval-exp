package org.saswata.expressions

import scala.collection.JavaConverters._

object Utils {

  def jmap2map(jmap: java.util.Map[java.lang.String, java.lang.Object]): Map[String, Any] =
    jmap.asScala.toMap

  def collectStrings(set: Set[_]): Set[String] = set.collect { case s: String => s }

  def sanitiseValues(env: Map[String, Any]): Map[String, Any] =
    env.filter(_._2 != null).collect {
      case (k: String, b: Boolean)                   => (k, b)
      case (k: String, n: java.lang.Number)          => (k, n.doubleValue())
      case (k: String, s: String)                    => (k, s)
      case (k: String, jIter: java.lang.Iterable[_]) => (k, collectStrings(jIter.asScala.toSet))
      case (k: String, seq: Seq[_])                  => (k, collectStrings(seq.toSet))
      case (k: String, set: Set[_])                  => (k, collectStrings(set))
    }
}
