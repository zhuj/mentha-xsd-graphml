package org.mentha.tools.xsd

import org.apache.commons.lang3.StringUtils

import scala.xml._
import collection.convert.wrapAsScala._
import org.apache.xerces.xs._

/** */
abstract class GraphMLRenderer {

  private[xsd] val straightness = "0.1"
  
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
        {for { n <- nodes } yield { node_tag(n) }}
        {for { n <- nodes; e <- n.outgoing if nodeIds.contains(e.dst.id) } yield { edge_tag(e) }}
      </graph>
    </graphml>
  }

  private[xsd] def node_tag(node: XSNode): Node =
    <node id={node.id}>
      <data key="d6">
        { node_tag_content(node) }
      </data>
    </node>

  private[xsd] def node_tag_content(node: XSNode): Node = node.obj match {
    case o: XSSimpleTypeDefinition => node__simple_type_definition(node, o)
    case o: XSComplexTypeDefinition => node__complex_type_definition(node, o)
    case o: XSElementDeclaration => node__element_declaration(node, o)
    case o: XSModelGroup => node__model_group(node, o)
    case o: XSWildcard => node__wildcard(node, o)
    case _ => node__generic(node)
  }

  private[xsd] def label(obj: XSObject): String = obj match {
    case o: XSTypeDefinition => if (o.getAnonymous) "(anonymous)" else name(o)
    case o: XSModelGroup => o.getCompositor match {
      case XSModelGroup.COMPOSITOR_SEQUENCE => "..."
      case XSModelGroup.COMPOSITOR_CHOICE => "?"
      case XSModelGroup.COMPOSITOR_ALL => "*"
    }
    case _ => name(obj)
  }

  private[xsd] def name(obj: XSObject) = obj.getName

  private[xsd] def node__simple_type_definition(node: XSNode, o: XSSimpleTypeDefinition) =
    <y:ShapeNode>
      <y:Geometry height="20" width="200" x="0" y="0"/>
      <y:Fill color="#e8eef7" transparent="false"/>
      <y:BorderStyle color="#677993" type="line" width="1.0"/>
      { node_label( label = label(o) ) }
      <y:Shape type="roundrectangle"/>
    </y:ShapeNode>

  private[xsd] def node__complex_type_definition_base(
    typeDef: XSComplexTypeDefinition,
    label: String,
    cgColor: String,
    bgColor: String
  ) = {
    val attributes = typeDef.getAttributeUses
      .map { x => x.asInstanceOf[XSAttributeUse] }
      .map { x => "• " + Option(x.getName).getOrElse(x.getAttrDeclaration.getName) }

    val height = 25 + 15 * attributes.size + { if (attributes.size > 0) 10 else 0 }

    <y:GenericNode configuration="com.yworks.entityRelationship.big_entity">
      <y:Geometry height={String.valueOf(height)} width="200" x="0" y="0"/>
      <y:Fill color={bgColor} transparent="false"/>
      <y:BorderStyle color="#000000" type="line" width="1.0"/>
      {
      node_label( label = label ) %
        Attribute("", "backgroundColor", {cgColor}, Null) %
        Attribute("", "hasBackgroundColor", "true", Null) %
        Attribute("", "modelName", "internal", Null) %
        Attribute("", "modelPosition", "t", Null)
      }
      {
      node_label(
        label = StringUtils.join(attributes.toArray[AnyRef], "\n"),
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

  private[xsd] def node__complex_type_definition(node: XSNode, o: XSComplexTypeDefinition) = {
    node__complex_type_definition_base(
      typeDef = o,
      label = label(o),
      cgColor = "#B7C9E3",
      bgColor = "#E8EEF7"
    )
  }

  private[xsd] def node__element_declaration(node: XSNode, o: XSElementDeclaration) = {

    lazy val hasTypeDef: Boolean = node
      .outgoing
      .exists(o => o.edgeType == XSEdgeType.ElementType)

    Option(o.getTypeDefinition) match {
      case Some(x: XSComplexTypeDefinition) if !hasTypeDef => node__complex_type_definition_base(
        typeDef = x,
        label = if (o.getAbstract) s"<< ${label(o)} >>" else label(o),
        cgColor = if (o.getAbstract) "#fffddf" else "#fff0b4",
        bgColor = "#fffddf"
      )
      case _ => {
        node__element_declaration_simple(node, o)
      }
    }
  }

  private[xsd] def node__element_declaration_simple(node: XSNode, o: XSElementDeclaration) =
    <y:ShapeNode>
      <y:Geometry height="25" width="200" x="0" y="0"/>
      {
        if (o.getAbstract) {
            <y:Fill color="#fffddf" transparent="false"/>
            <y:BorderStyle color="#808080" type="line" width="1.0"/>
        } else {
            <y:Fill color="#fff0b4" transparent="false"/>
            <y:BorderStyle color="#000000" type="line" width="1.0"/>
        }
      }
      {
        if (o.getAbstract) {
          node_label( label = s"<< ${label(o)} >>" ) % Attribute("", "textColor", "#5f5f5f", Null)
        } else {
          node_label( label = label(o) )
        }
      }
      <y:Shape type="roundrectangle"/>
    </y:ShapeNode>

  private[xsd] def node__model_group(node: XSNode, o: XSModelGroup) =
    <y:ShapeNode>
      <y:Geometry height="12" width="12" x="0" y="0"/>
      <y:Fill color="#FFFFFF" transparent="false"/>
      <y:BorderStyle color="#000000" type="line" width="1.0"/>
      { node_label( label = label(o) ) }
      <y:Shape type="roundrectangle"/>
    </y:ShapeNode>

  private[xsd] def node__wildcard(node: XSNode, o: XSWildcard) =
    <y:ShapeNode>
      <y:Geometry height="15" width="100" x="0" y="0"/>
      <y:Fill color="#e1ffe1" transparent="false"/>
      <y:BorderStyle color="#808080" type="line" width="1.0"/>
      { node_label( label = "WILDCARD" ) }
      <y:Shape type="hexagon"/>
    </y:ShapeNode>

  private[xsd] def node__generic(node: XSNode) =
    <y:ShapeNode>
      <y:Geometry height="15" width="100" x="0" y="0"/>
      <y:Fill color="#FFFFFF" transparent="false"/>
      <y:BorderStyle color="#000000" type="line" width="1.0"/>
      { node_label( label = label(node.obj) ) }
      <y:Shape type="roundrectangle"/>
    </y:ShapeNode>

  private[xsd] def node_label(label: String, nodes: Node*): Elem =
    <y:NodeLabel autoSizePolicy="content" fontFamily="Dialog" fontSize="12" fontStyle="plain" textColor="#000000"
                 hasBackgroundColor="false" hasLineColor="false"
                 horizontalTextPosition="center" verticalTextPosition="center" alignment="center"
                 modelName="custom" visible="true" width="10" height="12" x="0" y="0">{label}{nodes}</y:NodeLabel>


  private[xsd] def edge_tag(edge: XSEdge): Node =
    <edge id={edge.src.id + '-' + edge.dst.id} source={edge.src.id} target={edge.dst.id}>
      <data key="d10">
        { edge_tag_content(edge) }
      </data>
    </edge>

  private[xsd] def edge_tag_content(edge: XSEdge): Node = edge.edgeType match {
      case tp: XSEdgeType.BaseType => edge__base_type(edge, tp)
      case tp: XSEdgeType.Cardinality => edge__cardinality(edge, tp)
      case XSEdgeType.ElementType => edge__element_type(edge)
      case XSEdgeType.ElementSubstitution => edge__substitution(edge)
      case XSEdgeType.Association => edge__association(edge)
      case _ => edge__generic(edge)
    }

  private[xsd] def edge__base_type(edge: XSEdge, tp: XSEdgeType.BaseType) = edge_curve(
    edge,
    <y:Path sx="0.0" sy="0.0" tx="0.0" ty="0.0"/>,
    <y:LineStyle color="#993300" type="line" width="2.0"/>,
    <y:Arrows source="none" target="white_delta"/>,
    edge_label(tp.title) % Attribute("", "lineColor", "#993300", Null)
  )

  private[xsd] def edge__substitution(edge: XSEdge) = edge_curve(
    edge,
    <y:Path sx="0.0" sy="0.0" tx="0.0" ty="0.0"/>,
    <y:LineStyle color="#000080" type="line" width="2.0"/>,
    <y:Arrows source="none" target="white_delta"/>,
    edge_label("substituted-by") % Attribute("", "lineColor", "#000080", Null)
  )
  private[xsd] def edge__cardinality(edge: XSEdge, tp: XSEdgeType.Cardinality) = edge_curve(
    edge,
    <y:Path sx="0.0" sy="0.0" tx="0.0" ty="0.0"/>,
    <y:LineStyle color="#000000" type="line" width="1.0"/>,
    <y:Arrows source="none" target="standard"/>,
    edge_label(tp.title)
  )

  private[xsd] def edge__element_type(edge: XSEdge) = edge_curve(
    edge,
    <y:LineStyle color="#000000" type="line" width="1.0"/>,
    <y:Arrows source="none" target="t_shape"/>
  )

  private[xsd] def edge__generic(edge: XSEdge) = edge_curve(
    edge,
    <y:LineStyle color="#000000" type="line" width="1.0"/>,
    <y:Arrows source="none" target="standard"/>
  )

  private[xsd] def edge__association(edge: XSEdge) = edge_curve(
    edge,
      <y:LineStyle color="#000000" type="dashed" width="1.0"/>,
      <y:Arrows source="none" target="none"/>
  )

  private[xsd] def edge_curve(code: XSEdge, nodes: Node*): Elem =
    <y:QuadCurveEdge straightness={straightness}>
      <y:Path sx="0.0" sy="0.0" tx="0.0" ty="0.0"/>
      { nodes }
    </y:QuadCurveEdge>


  private[xsd] def edge_label(title: String): Elem =
    <y:EdgeLabel
    alignment="center"
    backgroundColor="#FFFFFF" distance="0.0" fontFamily="Dialog" fontSize="8" fontStyle="plain" textColor="#000000"
    horizontalTextPosition="center" verticalTextPosition="bottom"
    iconTextGap="4" lineColor="#000000" modelName="centered" modelPosition="center" preferredPlacement="anywhere" ratio="0.5"
    visible="true" width="10" height="10" x="0" y="0">{title}</y:EdgeLabel>

}

object GraphMLRenderer extends GraphMLRenderer {

}