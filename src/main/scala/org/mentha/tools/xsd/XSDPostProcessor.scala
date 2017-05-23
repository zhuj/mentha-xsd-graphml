package org.mentha.tools.xsd

import org.apache.xerces.xs._
import org.mentha.tools.xsd.Utils.flatExtend

import scala.annotation.tailrec
import scala.util.Try

/** */
object XSDPostProcessor {

  @tailrec
  private def reduce(nodes: Seq[XSNode])(core: Seq[XSNode] => Option[Seq[XSNode]]): Seq[XSNode] = {
    core(nodes) match {
      case Some(next) => reduce(next)(core)
      case None => nodes
    }
  }

  private def remove(nodes: Seq[XSNode], removed: Set[String]) = {
    if (removed.isEmpty) None
    else Some(nodes.filterNot { n => removed.contains(n.id) })
  }

  // TODO: comment, use-cases
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
            .filter { e => anyTypeNodeIds.contains(e.dst.id) }
            .filter { e => e.edgeType == XSEdgeType.BaseType(XSConstants.DERIVATION_RESTRICTION) }
            .foreach { e => node.remove(e) }
        }
      }

    nodes
  }

  // TODO: comment, use-cases
  def simplifyModelGroups(nodes: Seq[XSNode]): Seq[XSNode] = reduce(nodes) {
    nodes => {

      def mix(incomingType: XSEdgeType.Cardinality, outgoingType: XSEdgeType.Cardinality): XSEdgeType.Cardinality = {
        XSEdgeType.Cardinality(
          math.min(incomingType.min, outgoingType.min),
          math.max(incomingType.max, outgoingType.max)
        )
      }

      val removed = {
        // filter empty
        nodes
          .filter { n => n.obj.isInstanceOf[XSModelGroup] }
          .filter { n => n.outgoing.isEmpty }
          .collect {
            case node => {
              nodes
                .incoming(node)
                .foreach { case (n, e) => n.remove(e) }
              node.id
            }
          }
          .toSet
      } ++ {
        // filter single-outgoing
        nodes
          .filter { n => n.obj.isInstanceOf[XSModelGroup] }
          .filter { n => 1 == n.outgoing.size }
          .collect {
            case node => {
              val outgoing = node.outgoing.iterator.next()
              nodes
                .incoming(node)
                .foreach {
                  case (n, e) => n.replace(e, outgoing.dst, mix(
                    incomingType = e.edgeType.asInstanceOf[XSEdgeType.Cardinality],
                    outgoingType = outgoing.edgeType.asInstanceOf[XSEdgeType.Cardinality]
                  ))
                }
              node.id
            }
          }
          .toSet
      }

      remove(nodes, removed)
    }
  }

  // TODO: comment, use-cases
  def removeModelGroups(nodes: Seq[XSNode]): Seq[XSNode] = {

    nodes
      .foreach {
        node =>
          node.outgoing
            .filter { e => e.edgeType.isInstanceOf[XSEdgeType.Cardinality] }
            .foreach { e => node.replace(e, e.dst, XSEdgeType.SimpleLink) }
      }

    reduce(nodes) {
      nodes => {

        val removed = nodes
          .filter { n => n.obj.isInstanceOf[XSModelGroup] }
          .collect {
            case node => {
              nodes.incoming(node)
                .foreach {
                  case (n, i) => {
                    n.remove(i)
                    node.outgoing
                      .foreach { o => n.link(o.dst, XSEdgeType.SimpleLink) }
                  }
                }

              node.id
            }
          }
          .toSet[String]

        remove(nodes, removed)
      }
    }
  }

  // TODO: comment, use-cases
  def inlineElementTypes(nodes: Seq[XSNode], all: Boolean = false): Seq[XSNode] = reduce(nodes) {
    nodes => {

      val removed = nodes
        .filter { n => n.obj.isInstanceOf[XSTypeDefinition] }
        .filter { n => all || n.obj.asInstanceOf[XSTypeDefinition].getAnonymous }
        .map { n => (n -> nodes.incoming(n)) }
        .collect {
          case (node, incoming) if (incoming.size == 1) => {
            incoming
              .foreach {
                case (n, i) => {
                  n.remove(i)
                  node.outgoing
                    .foreach { o => n.link(o.dst, o.edgeType) }
                }
              }
            node.id
          }
        }
        .toSet[String]

      remove(nodes, removed)
    }
  }

  type XSPoint = (XSNode, XSNode, List[XSEdge])

  // TODO: comment
  def achievable(edgeFilter: XSEdge => Boolean)(src: XSNode*): Stream[XSPoint] = {
    def core(front: Stream[XSPoint], visited: Set[String]): Stream[XSPoint] = {
      val (fnext, vnext) = flatExtend[XSPoint, String](
        stream = front,
        visited = visited,
        extend = {
          case ((src, node, path), visited) =>
            for {
              edge <- node.outgoing if edgeFilter(edge) && !visited.contains(edge.dst.id)
            } yield (src, edge.dst, edge :: path)
        },
        id = { case (_, node, _) => node.id }
      )

      if (fnext.isEmpty) Stream.Empty
      else fnext #::: core(
        front = fnext,
        visited = vnext
      )
    }

    core(
      front=src.map { s => (s, s, Nil) }.toStream,
      visited=Set() // cycles are possible
    )
    //.map { case (n, path) => (n, path.reverse) }
  }

}
