package org.mentha.tools.xsd

import org.apache.commons.lang3.StringUtils

import scala.xml._
import collection.convert.wrapAsScala._
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
        {for { n <- nodes; e <- n.outgoing if nodeIds.contains(e.dstId) } yield { edge(n, e) }}
      </graph>
    </graphml>
  }

  private def node(node: XSNode) =
    <node id={node.id}>
      <data key="d6">
        {
          node.obj match {
            case o: XSSimpleTypeDefinition => node_simple_type_definition(node, o)
            case o: XSComplexTypeDefinition => node_complex_type_definition_adv(node, o)
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
      { node_label(o.getName) }
      <y:Shape type="roundrectangle"/>
    </y:ShapeNode>

  private def node_complex_type_definition(node: XSNode, o: XSComplexTypeDefinition) =
    <y:ShapeNode>
      <y:Geometry height="20" width="200" x="0" y="0"/>
      <y:Fill color="#b7c9e3" transparent="false"/>
      <y:BorderStyle color="#677993" type="line" width="1.0"/>
      { node_label(o.getName) }
      <y:Shape type="roundrectangle"/>
    </y:ShapeNode>

  private def node_complex_type_definition_adv(node: XSNode, o: XSComplexTypeDefinition) = {
    val attributes = o.getAttributeUses
      .map { x => x.asInstanceOf[XSAttributeUse] }
      .map { x => "• " + Option(x.getName).orElse(Option(x.getAttrDeclaration.getName)).getOrElse("?") }

    val height = 25 + 15 * attributes.size + { if (attributes.size > 0) 10 else 0 }

    <y:GenericNode configuration="com.yworks.entityRelationship.big_entity">
      <y:Geometry height={String.valueOf(height)} width="200" x="0" y="0"/>
      <y:Fill color="#E8EEF7" transparent="false"/>
      <y:BorderStyle color="#000000" type="line" width="1.0"/>
      {
        node_label(Option(o.getName).getOrElse("(inline)")) %
          Attribute("", "backgroundColor", "#B7C9E3", Null) %
          Attribute("", "hasBackgroundColor", "true", Null) %
          Attribute("", "modelName", "internal", Null) %
          Attribute("", "modelPosition", "t", Null)
      }
      {
        node_label(
          StringUtils.join(attributes.toArray[AnyRef], "\n"),
          <y:LabelModel>
            <y:ErdAttributesNodeLabelModel/>
          </y:LabelModel>,
          <y:ModelParameter>
            <y:ErdAttributesNodeLabelModelParameter/>
          </y:ModelParameter>
        ) %
          Attribute("", "configuration", "com.yworks.entityRelationship.label.attributes", Null) %
          Attribute("", "alignment", "left", Null)
      }
      <y:StyleProperties>
      <y:Property class="java.lang.Boolean" name="y.view.ShadowNodePainter.SHADOW_PAINTING" value="true"/>
    </y:StyleProperties>
    </y:GenericNode>
  }

  private def node_element_declaration(node: XSNode, o: XSElementDeclaration) =
    <y:ShapeNode>
      <y:Geometry height="16" width="200" x="0" y="0"/>
      {
        if (o.getAbstract) {
            <y:Fill color="#fffddf" transparent="false"/>
        } else {
            <y:Fill color="#fff0b4" transparent="false"/>
        }
      }
      <y:BorderStyle color="#808080" type="line" width="1.0"/>
      {
        if (o.getAbstract) {
          node_label(s"<< ${o.getName} >>") % Attribute("", "textColor", "#5f5f5f", Null)
        } else {
          node_label(o.getName)
        }
      }
    </y:ShapeNode>

  private def node_model_group(node: XSNode, o: XSModelGroup) =
    <y:ShapeNode>
      <y:Geometry height="12" width="12" x="0" y="0"/>
      <y:Fill color="#FFFFFF" transparent="false"/>
      <y:BorderStyle color="#000000" type="line" width="1.0"/>
      { node_label(getModelGroupTitle(o)) }
      <y:Shape type="roundrectangle"/>
    </y:ShapeNode>

  private def node_wildcard(node: XSNode, o: XSWildcard) =
    <y:ShapeNode>
      <y:Geometry height="15" width="100" x="0" y="0"/>
      <y:Fill color="#e1ffe1" transparent="false"/>
      <y:BorderStyle color="#808080" type="line" width="1.0"/>
      { node_label( "WILDCARD") }
      <y:Shape type="hexagon"/>
    </y:ShapeNode>

  private def node_generic(node: XSNode) =
    <y:ShapeNode>
      <y:Geometry height="15" width="100" x="0" y="0"/>
      <y:Fill color="#FFFFFF" transparent="false"/>
      <y:BorderStyle color="#000000" type="line" width="1.0"/>
      { node_label(node.obj.getName) }
      <y:Shape type="roundrectangle"/>
    </y:ShapeNode>

  private def node_label(label: String, nodes: Node*) =
    <y:NodeLabel autoSizePolicy="content" fontFamily="Dialog" fontSize="12" fontStyle="plain" textColor="#000000"
                 hasBackgroundColor="false" hasLineColor="false"
                 horizontalTextPosition="center" verticalTextPosition="center" alignment="center"
                 modelName="custom" visible="true" width="10" height="12" x="0" y="0">{label}{nodes}</y:NodeLabel>


  private def edge(node: XSNode, edge: XSEdge) =
    <edge id={node.id + '-' + edge.dstId} source={node.id} target={edge.dstId}>
      <data key="d10">
        {
          edge.edgeType match {
            case tp: XSEdgeType.BaseType => edge_parent(edge, tp)
            case tp: XSEdgeType.Cardinality => edge_cardinality(edge, tp)
            case XSEdgeType.ElementSubstitution => edge_substitution(edge)
            case _ => edge_generic(edge)
          }
        }
      </data>
    </edge>

  private def edge_parent(edge: XSEdge, tp: XSEdgeType.BaseType) =
    <y:QuadCurveEdge straightness="0.1">
      <y:Path sx="0.0" sy="0.0" tx="0.0" ty="0.0"/>
      <y:LineStyle color="#993300" type="line" width="2.0"/>
      <y:Arrows source="none" target="white_delta"/>
      { edge_label(tp.title, "#993300") }
    </y:QuadCurveEdge>

  private def edge_substitution(edge: XSEdge) =
    <y:QuadCurveEdge straightness="0.1">
      <y:Path sx="0.0" sy="0.0" tx="0.0" ty="0.0"/>
      <y:LineStyle color="#000080" type="line" width="2.0"/>
      <y:Arrows source="none" target="white_delta"/>
      { edge_label("substituted-by", "#000080") }
    </y:QuadCurveEdge>

  private def edge_cardinality(edge: XSEdge, tp: XSEdgeType.Cardinality) =
    <y:QuadCurveEdge straightness="0.1">
      <y:Path sx="0.0" sy="0.0" tx="0.0" ty="0.0"/>
      <y:LineStyle color="#000000" type="line" width="1.0"/>
      <y:Arrows source="none" target="standard"/>
      { edge_label(tp.title) }
    </y:QuadCurveEdge>

  private def edge_generic(edge: XSEdge) =
    <y:QuadCurveEdge straightness="0.1">
      <y:Path sx="0.0" sy="0.0" tx="0.0" ty="0.0"/>
      <y:LineStyle color="#000000" type="line" width="1.0"/>
      <y:Arrows source="none" target="standard"/>
    </y:QuadCurveEdge>

  private def edge_label(title: String, lineColor: String="#000000") =
    <y:EdgeLabel
    alignment="center"
    backgroundColor="#FFFFFF" distance="0.0" fontFamily="Dialog" fontSize="8" fontStyle="plain" textColor="#000000"
    horizontalTextPosition="center" verticalTextPosition="bottom"
    iconTextGap="4" lineColor={lineColor} modelName="centered" modelPosition="center" preferredPlacement="anywhere" ratio="0.5"
    visible="true" width="10" height="10" x="0" y="0">{title}</y:EdgeLabel>



  private def getModelGroupTitle(o: XSModelGroup): String =
    o.getCompositor match {
      case XSModelGroup.COMPOSITOR_SEQUENCE => "..."
      case XSModelGroup.COMPOSITOR_CHOICE => "?"
      case XSModelGroup.COMPOSITOR_ALL => "*"
    }

}
