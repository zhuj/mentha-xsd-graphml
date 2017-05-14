package org.mentha.tools.xsd

import org.apache.xerces.xs.XSModelGroup

import scala.collection.mutable
import scala.util.control.Breaks._

/** */
object XSDPostProcessor {

  def simplifyModelGroups(nodes: Seq[XSNode]): Seq[XSNode] = {

    def core(nodes: Array[XSNode]): Option[Array[XSNode]] = {

      val removed = mutable.Set[String]()
      nodes
        .filter { n => n.obj.isInstanceOf[XSModelGroup] }
        .filter { n => 1 == n.outgoing.size }
        .foreach {
          node => {
            val outgoing = node.outgoing.iterator.next()
            outgoing.edgeType match {
              case cardinality: XSEdgeType.Cardinality if cardinality.simple => {
                removed += node.id
                nodes
                  .flatMap { n => n.outgoing.filter { e => node == e.dst }.map { e => (n, e) } }
                  .foreach { case (n, e) => n.replace(e, outgoing.dst, e.edgeType) }
              }
              case _ => {
              }
            }
          }
        }

      if (removed.isEmpty) None
      else Some(nodes.filterNot { n => removed.contains(n.id) })
    }

    var n = nodes.toArray
    breakable {
      while (true) {
        core(n) match {
          case Some(next) => n = next
          case None => break()
        }
      }
    }

    n
  }

}
