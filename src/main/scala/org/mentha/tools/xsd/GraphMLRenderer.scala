package org.mentha.tools.xsd

import org.apache.xerces.xs._

/** */
object GraphMLRenderer {

  def render(nodes: Seq[XSNode]) = {
    val nodeIds = nodes
      .map { n => n.id }
      .toSet

    <graphml
    xmlns="http://graphml.graphdrawing.org/xmlns"
    xmlns:java="http://www.yworks.com/xml/yfiles-common/1.0/java"
    xmlns:sys="http://www.yworks.com/xml/yfiles-common/markup/primitives/2.0"
    xmlns:x="http://www.yworks.com/xml/yfiles-common/markup/2.0"
    xmlns:y="http://www.yworks.com/xml/graphml"
    xmlns:yed="http://www.yworks.com/xml/yed/3"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd">

      <key attr.name="Description" attr.type="string" for="graph" id="d0"/>
      <key for="port" id="d1" yfiles.type="portgraphics"/>
      <key for="port" id="d2" yfiles.type="portgeometry"/>
      <key for="port" id="d3" yfiles.type="portuserdata"/>
      <key attr.name="url" attr.type="string" for="node" id="d4"/>
      <key attr.name="description" attr.type="string" for="node" id="d5"/>
      <key for="node" id="d6" yfiles.type="nodegraphics"/>
      <key for="graphml" id="d7" yfiles.type="resources"/>
      <key attr.name="url" attr.type="string" for="edge" id="d8"/>
      <key attr.name="description" attr.type="string" for="edge" id="d9"/>
      <key for="edge" id="d10" yfiles.type="edgegraphics"/>

      <graph edgedefault="directed" id="G">
        <data key="d0"/>
        {for { n <- nodes } yield { node(n) }}
        {for { n <- nodes; e <- n.outgoing if nodeIds.contains(e.dst.id) } yield { edge(n, e) }}
      </graph>
    </graphml>
  }

  private def node(node: XSNode) =
    <node id={node.id}>
      <data key="d6">
        {
          node.obj match {
            case o: XSSimpleTypeDefinition => node_simple_type_definition(node, o)
            case o: XSComplexTypeDefinition => node_complex_type_definition(node, o)
            case o: XSElementDeclaration => node_element_declaration(node, o)
            case o: XSModelGroup => node_model_group(node, o)
            case o: XSWildcard => node_wildcard(node, o)
            case _ => node_generic(node)
          }
        }
      </data>
    </node>

  private def node_simple_type_definition(node: XSNode, o: XSSimpleTypeDefinition) =
    <y:ShapeNode>
      <y:Geometry height="20" width="200" x="0" y="0"/>
      <y:Fill color="#e8eef7" transparent="false"/>
      <y:BorderStyle color="#677993" type="line" width="1.0"/>
      { node_label(o.getName, 200) }
      <y:Shape type="roundrectangle"/>
    </y:ShapeNode>

  private def node_complex_type_definition(node: XSNode, o: XSComplexTypeDefinition) =
    <y:ShapeNode>
      <y:Geometry height="20" width="200" x="0" y="0"/>
      <y:Fill color="#b7c9e3" transparent="false"/>
      <y:BorderStyle color="#677993" type="line" width="1.0"/>
      { node_label(o.getName, 200) }
      <y:Shape type="roundrectangle"/>
    </y:ShapeNode>

  private def node_element_declaration(node: XSNode, o: XSElementDeclaration) =
    <y:ShapeNode>
      <y:Geometry height="16" width="200" x="0" y="0"/>
      <y:Fill color="#fff0b4" transparent="false"/>
      <y:BorderStyle color="#808080" type="line" width="1.0"/>
      { node_label(o.getName, 200) }
    </y:ShapeNode>

  private def node_model_group(node: XSNode, o: XSModelGroup) =
    <y:ShapeNode>
      <y:Geometry height="12" width="12" x="0" y="0"/>
      <y:Fill color="#FFFFFF" transparent="false"/>
      <y:BorderStyle color="#000000" type="line" width="1.0"/>
      { node_label(getModelGroupTitle(o), 20) }
      <y:Shape type="roundrectangle"/>
    </y:ShapeNode>

  private def node_wildcard(node: XSNode, o: XSWildcard) =
    <y:ShapeNode>
      <y:Geometry height="15" width="100" x="0" y="0"/>
      <y:Fill color="#e1ffe1" transparent="false"/>
      <y:BorderStyle color="#808080" type="line" width="1.0"/>
      { node_label( "WILDCARD", 100) }
      <y:Shape type="hexagon"/>
    </y:ShapeNode>

  private def node_generic(node: XSNode) =
    <y:ShapeNode>
      <y:Geometry height="15" width="100" x="0" y="0"/>
      <y:Fill color="#FFFFFF" transparent="false"/>
      <y:BorderStyle color="#000000" type="line" width="1.0"/>
      { node_label(node.obj.getName, 100) }
      <y:Shape type="roundrectangle"/>
    </y:ShapeNode>

  private def node_label(label: String, width: Int) =
    <y:NodeLabel autoSizePolicy="content" fontFamily="Dialog" fontSize="12" fontStyle="plain" textColor="#000000"
                 hasBackgroundColor="false" hasLineColor="false"
                 horizontalTextPosition="center" verticalTextPosition="center" alignment="center"
                 modelName="custom" visible="true" width={width.toString} height="12" x="0" y="0">{label}</y:NodeLabel>


  private def edge(node: XSNode, edge: XSEdge) =
    <edge id={node.id + '-' + edge.dst.id} source={node.id} target={edge.dst.id}>
      <data key="d10">
        {
          edge.edgeType match {
            case XSEdgeType.BaseTypeEdge => edge_parent(edge)
            case tp: XSEdgeType.Cardinality => edge_cardinality(edge, tp)
            case _ => edge_generic(edge)
          }
        }
      </data>
    </edge>

  private def edge_parent(edge: XSEdge) =
    <y:QuadCurveEdge straightness="0.1">
      <y:Path sx="0.0" sy="0.0" tx="0.0" ty="0.0"/>
      <y:LineStyle color="#993300" type="line" width="2.0"/>
      <y:Arrows source="none" target="white_delta"/>
    </y:QuadCurveEdge>

  private def edge_cardinality(edge: XSEdge, tp: XSEdgeType.Cardinality) =
    <y:QuadCurveEdge straightness="0.1">
      <y:Path sx="0.0" sy="0.0" tx="0.0" ty="0.0"/>
      <y:LineStyle color="#000000" type="line" width="1.0"/>
      <y:Arrows source="none" target="standard"/>
      { edge_label(tp.title.get) }
    </y:QuadCurveEdge>

  private def edge_generic(edge: XSEdge) =
    <y:QuadCurveEdge straightness="0.1">
      <y:Path sx="0.0" sy="0.0" tx="0.0" ty="0.0"/>
      <y:LineStyle color="#000000" type="line" width="1.0"/>
      <y:Arrows source="none" target="standard"/>
    </y:QuadCurveEdge>

  private def edge_label(title: String) =
    <y:EdgeLabel
    alignment="center"
    backgroundColor="#FFFFFF" distance="0.0" fontFamily="Dialog" fontSize="8" fontStyle="plain" textColor="#000000"
    horizontalTextPosition="center" verticalTextPosition="bottom"
    iconTextGap="4" lineColor="#000000" modelName="centered" modelPosition="center" preferredPlacement="anywhere" ratio="0.5"
    visible="true" width="10" height="10" x="0" y="0">{title}</y:EdgeLabel>



  private def getModelGroupTitle(o: XSModelGroup): String =
    o.getCompositor match {
      case XSModelGroup.COMPOSITOR_SEQUENCE => "..."
      case XSModelGroup.COMPOSITOR_CHOICE => "?"
      case XSModelGroup.COMPOSITOR_ALL => "*"
    }

}
