package org.mentha.tools.xsd

import java.io.File
import java.nio.charset.Charset

import org.apache.commons.io.FileUtils
import org.apache.xerces.xs._

import scala.collection.convert.wrapAsScala._
import scala.collection.mutable

/** */
abstract class XSDCollation {

  val a_source_uri: String
  val a_namespace: String

  val b_source_uri: String
  val b_namespace: String

  /**
    *
    * @param obj
    * @return
    */
  def fullName(obj: XSObject): String

  /**
    *
    * @param node
    * @return
    */
  def fullName(node: XSNode): String = this.fullName(node.obj)

  /**
    *
    * @return
    */
  def loadNodes: Seq[XSNode] = {
    val processor = new XSDProcessor()
    load_model(a_source_uri, a_namespace)(processor)
    load_model(b_source_uri, b_namespace)(processor)
    processor.nodes
  }

  /**
    *
    * @param sourceUri
    * @param namespace
    * @param processor
    */
  private def load_model(sourceUri: String, namespace: String)(processor: XSDProcessor): Unit = {
    val model = XSDParser.loadModel(sourceUri)
    val components = model.getComponentsByNamespace(XSConstants.ELEMENT_DECLARATION, namespace)
    components.values()
      .map { obj => obj.asInstanceOf[XSElementDeclaration] }
      .foreach { obj =>
        processor.process(obj)
      }
  }

  /**
    *
    */
  lazy val nodes: Seq[XSNode] = {
    var nodes = loadNodes
    nodes = XSDPostProcessor.clearRestrictAnyType(nodes)
    nodes = XSDPostProcessor.simplifyModelGroups(nodes)
    nodes = XSDPostProcessor.removeModelGroups(nodes)
    nodes = XSDPostProcessor.inlineElementTypes(nodes, all=true)

    if (true) {
      val graphMl = GraphMLRenderer.render(nodes)
      FileUtils.write(
        new File(s"${a_source_uri}.collate.graphml"),
        graphMl.toString,
        Charset.forName("UTF-8")
      )
    }

    nodes
  }

  /**
    *
    */
  lazy val nodeNameMap: Map[String, XSNode] = nodes
    .filter { node => node.obj.isInstanceOf[XSElementDeclaration] }
    .groupBy { node => fullName(node) }
    .mapValues {
      case v if v.size == 1 => v.iterator.next()
      case v => throw new IllegalStateException("Duplicate fullName for: " + v)
    }

  /**
    *
    * @param src_names
    * @param dst_names
    * @return Option ( src, dst, { edge0 -> .... -> edgeN | src -> edge0 -> ... -> edgeN -> dst  } )
    */
  private def trace(src_names: Seq[String], dst_names: Seq[String]): Option[(XSNode, XSNode, List[XSEdge])] = {
    val b_src = src_names .flatMap { name => nodeNameMap.get(name) } .toSeq
    val b_dst = dst_names .flatMap { name => nodeNameMap.get(name) } .toSet
    XSDPostProcessor
      .achievable { case e => e.edgeType != XSEdgeType.Association }(b_src:_*)
      .collectFirst { case (src, dst, path) if b_dst.contains(dst) => (src, dst, path) }
  }

  /**
    *
    * @param collationMap
    * @return { node -> { edge | there is no possible route for edge with given collation }}
    */
  def collate(collationMap: Map[String, Seq[String]]): Map[XSNode, Seq[XSEdge]] = {

    for {
      (a_name, b_names) <- collationMap
      b_name <- b_names
    } {
      val a_node = nodeNameMap(a_name)
      val b_node = nodeNameMap(b_name)
      a_node.link(b_node, XSEdgeType.Association)
    }

    val missing_edges = mutable.Map[XSNode, Seq[XSEdge]]()
    val unique_names = mutable.Set[String]()

    for {
      (a_name, b_names) <- collationMap
      a_src = nodeNameMap(a_name)
    } {

      val a_achievable: Stream[(XSNode, List[XSEdge])] = XSDPostProcessor
        .achievable { case e => e.edgeType != XSEdgeType.Association }(a_src)
        .map { case (_, node, path) => (node -> path) }

      type Trace = (XSNode, List[XSEdge], XSNode, XSNode, List[XSEdge])
      val b_traces_all: Stream[Trace] = {
        for {
          (a_dst, a_path) <- a_achievable
          b_dstNames <- collationMap.get(fullName(a_dst))
          (b_src, b_dst, b_path) <- trace(b_names, b_dstNames)
        } yield (a_dst, a_path, b_src, b_dst, b_path)
      }

      val b_traces: Stream[Trace] = Utils.filter[Trace, String](
        stream = b_traces_all,
        extend = {
          case ((a_dst, a_path, b_src, b_dst, b_trace), visited) =>
            if (a_path.map { e => fullName(e.dst) }.exists(name => visited.contains(name))) Stream.Empty
            else Stream((a_dst, a_path, b_src, b_dst, b_trace))
        },
        id = {
          case (a_dst, _, _, _, _) => fullName(a_dst)
        }
      )

      var all_edges = a_src
        .outgoing
        .filter { e => e.edgeType != XSEdgeType.Association }
        .toSet

      for {
        (a_dst, a_path, b_src, b_dst, b_path) <- b_traces
      } {
        all_edges = all_edges - a_path.head

        val unique_name = s"${a_src.obj.getName}-${a_dst.obj.getName}"
        if (!unique_names.add(unique_name)) {
          throw new IllegalStateException(s"Duplicate name: ${unique_name}")
        }

        if (true) {
          val a_trace = a_path.foldLeft(Set[XSNode]()) { case (set, e) => set + e.src + e.dst }
          val b_trace = b_path.foldLeft(Set[XSNode]()) { case (set, e) => set + e.src + e.dst }

          val a_nodes = Set(a_src, a_dst)
          val b_nodes = Set(b_src, b_dst)

          val allNodes: Seq[XSNode] = {
            a_nodes ++ a_trace ++ b_nodes ++ b_trace
          }.toSeq

          val renderer = new GraphMLRenderer {
            override val straightness = "0.75"

            override def node_tag_content(n: XSNode) =
              if (a_nodes.contains(n)) super.node_tag_content(n)
              else super.node__generic(n)

            override def edge_tag(edge: XSEdge) =
              super.edge_tag(edge)

            override def name(obj: XSObject) =
              fullName(obj)

          }

          val graphMl = renderer.render(allNodes)
          FileUtils.write(
            new File(s"${a_source_uri}.collate/${unique_name}.graphml"),
            graphMl.toString,
            Charset.forName("UTF-8")
          )
        }
      }

      val missing = all_edges
        .toList
        .filter { e => e.edgeType != XSEdgeType.Association }

      if (!missing.isEmpty) {
        missing_edges += (a_src -> missing)
      }
    }

    missing_edges.toMap
  }

}
