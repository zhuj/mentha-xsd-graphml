package org.mentha.tools.xsd

import org.apache.xerces.xs._

import scala.annotation.tailrec
import scala.collection.mutable

/** */
object XSDPostProcessor {

  def clearRestrictAnyType(nodes: Seq[XSNode]): Seq[XSNode] = {

    def isAnyTypeDef(obj: XSObject): Boolean =
      obj.isInstanceOf[XSComplexTypeDefinition] &&
        ("http://www.w3.org/2001/XMLSchema" == obj.getNamespace) &&
        ("anyType" == obj.getName)


    val anyTypeNodeIds = nodes
      .collect { case n if isAnyTypeDef(n.obj) => n.id }
      .toSet

    nodes
      .filter { n => n.obj.isInstanceOf[XSComplexTypeDefinition] }
      .foreach {
        node => {
          node.outgoing
            .filter { e => anyTypeNodeIds.contains(e.dstId) }
            .filter { e => e.edgeType == XSEdgeType.BaseType(XSConstants.DERIVATION_RESTRICTION) }
            .foreach { e => node.remove(e) }
        }
      }

    nodes

  }

  def simplifyModelGroups(nodes: Seq[XSNode]): Seq[XSNode] = {

    def mix(incomingType: XSEdgeType.Cardinality, outgoingType: XSEdgeType.Cardinality): XSEdgeType.Cardinality = {
      XSEdgeType.Cardinality(
        math.min(incomingType.min, outgoingType.min),
        math.max(incomingType.max, outgoingType.max)
      )
    }

    def core(nodes: Seq[XSNode]): Option[Seq[XSNode]] = {

      val removed = mutable.Set[String]()

      // filter empty
      nodes
        .filter { n => n.obj.isInstanceOf[XSModelGroup] }
        .filter { n => n.outgoing.isEmpty }
        .foreach {
          node => {
            removed += node.id
            nodes
              .flatMap { n => n.outgoing.filter { e => node.id == e.dstId }.map { e => (n, e) } }
              .foreach { case (n, e) => n.remove(e) }
          }
        }

      // filter single-outgoing
      nodes
        .filter { n => n.obj.isInstanceOf[XSModelGroup] }
        .filter { n => 1 == n.outgoing.size }
        .foreach {
          node => {
            val outgoing = node.outgoing.iterator.next()
            removed += node.id
            nodes
              .flatMap { n => n.outgoing.filter { e => node.id == e.dstId }.map { e => (n, e) } }
              .foreach { case (n, e) => n.replace(e, outgoing.dstId, mix(
                incomingType = e.edgeType.asInstanceOf[XSEdgeType.Cardinality],
                outgoingType = outgoing.edgeType.asInstanceOf[XSEdgeType.Cardinality]
              )) }
          }
        }

      if (removed.isEmpty) None
      else Some(nodes.filterNot { n => removed.contains(n.id) })
    }

    @tailrec
    def reducer(nodes: Seq[XSNode]): Seq[XSNode] = {
      core(nodes) match {
        case Some(next) => reducer(next)
        case None => nodes
      }
    }

    reducer(nodes)
  }

}
