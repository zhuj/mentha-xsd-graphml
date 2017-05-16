package org.mentha.tools.xsd

import javax.validation.constraints.NotNull

import org.apache.commons.lang3.builder._
import org.apache.xerces.xs._

import scala.collection.mutable
import collection.convert.wrapAsScala._

trait XSEdgeType {
}

object XSEdgeType {

  object ElementType extends XSEdgeType {}
  object ElementSubstitution extends XSEdgeType {}
  object SimpleLink extends XSEdgeType {}


  case class BaseType(derivationMethod: Short) extends XSEdgeType {

    def title: String = derivationMethod match {
      case XSConstants.DERIVATION_NONE => "none"
      case XSConstants.DERIVATION_EXTENSION => "extends"
      case XSConstants.DERIVATION_RESTRICTION => "restricts"
      case XSConstants.DERIVATION_SUBSTITUTION => "substitutes"
      case XSConstants.DERIVATION_UNION => "unions"
      case XSConstants.DERIVATION_LIST => "lists"
      case _ => s"unknown:${derivationMethod}"
    }

  }

  case class Cardinality(min: Int, max: Int) extends XSEdgeType {

    def title: String = {
      if (min == max) String.valueOf(min)
      else {
        min + ".." + {
          if (max == Int.MaxValue) "*" else max
        }
      }
    }

    def simple: Boolean = (min == max) && (min == 1)

  }

  object Cardinality {
    def apply(particle: XSParticle): Cardinality = new Cardinality(
      particle.getMinOccurs,
      if (particle.getMaxOccursUnbounded) Int.MaxValue else particle.getMaxOccurs
    )
  }

}


/** */
case class XSEdge(dstId: String, edgeType: XSEdgeType) {

}

/** */
class XSNode(val obj: XSObject) {

  val id = XSNode.id(obj)

  private val outgoingEdges = mutable.Buffer[XSEdge]()

  def outgoing: Seq[XSEdge] = outgoingEdges.toSeq

  def typeName: String = XSNode
    .getTypeName(obj.getType)
    .getOrElse(obj.getClass.getSimpleName)

  def link(targetId: String, edgeType: XSEdgeType): XSEdge = {
    val edge = new XSEdge(targetId, edgeType)
    outgoingEdges += edge
    edge
  }

  def remove(e: XSEdge): Unit = {
    val i = outgoingEdges.indexOf(e)
    if (i < 0) { throw new IllegalStateException() }
    outgoingEdges.remove(i)
  }

  def replace(e: XSEdge, targetId: String, edgeType: XSEdgeType): Unit = {
    val i = outgoingEdges.indexOf(e)
    if (i < 0) { throw new IllegalStateException() }
    outgoingEdges(i) = new XSEdge(targetId, edgeType)
  }

  override def toString: String = new ToStringBuilder(this, ToStringStyle.SIMPLE_STYLE)
    .append("id", this.id)
    .append("object", this.obj)
    .toString

  override def hashCode: Int = new HashCodeBuilder()
    .append(this.id)
    .hashCode

  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[XSNode] && this.equals(obj.asInstanceOf[XSNode])
  }

  @inline
  private def equals(@NotNull that: XSNode) = new EqualsBuilder()
    .append(this.id, that.id)
    .isEquals

}

object XSNode {

  def id(obj: XSObject): String = java.lang.Long.toUnsignedString(
    System.identityHashCode(obj), 32
  )

  def getTypeName(tp: Short): Option[String] = tp match {
    case XSConstants.ATTRIBUTE_DECLARATION => Some("ATTRIBUTE_DECLARATION")
    case XSConstants.ELEMENT_DECLARATION => Some("ELEMENT_DECLARATION")
    case XSConstants.TYPE_DEFINITION => Some("TYPE_DEFINITION")
    case XSConstants.ATTRIBUTE_USE => Some("ATTRIBUTE_USE")
    case XSConstants.ATTRIBUTE_GROUP => Some("ATTRIBUTE_GROUP")
    case XSConstants.MODEL_GROUP_DEFINITION => Some("MODEL_GROUP_DEFINITION")
    case XSConstants.MODEL_GROUP => Some("MODEL_GROUP")
    case XSConstants.PARTICLE => Some("PARTICLE")
    case XSConstants.WILDCARD => Some("WILDCARD")
    case XSConstants.IDENTITY_CONSTRAINT => Some("IDENTITY_CONSTRAINT")
    case XSConstants.NOTATION_DECLARATION => Some("NOTATION_DECLARATION")
    case XSConstants.ANNOTATION => Some("ANNOTATION")
    case XSConstants.FACET => Some("FACET")
    case XSConstants.MULTIVALUE_FACET => Some("MULTIVALUE_FACET")
    case _ => None
  }

  implicit class XSNodeSeq(nodes: Seq[XSNode]) {

    def incoming(node: XSNode): Seq[(XSNode, XSEdge)] = nodes
      .flatMap { n => n.outgoing.filter { e => node.id == e.dstId }.map { e => (n, e) } }

    def buildMap: Map[String, XSNode] = nodes
      .map { n => (n.id -> n) }
      .toMap[String, XSNode]

  }

}


/** */
class XSDProcessor(model: XSModel) {

  private def $debug(arguments: AnyRef*) = {
    import org.apache.commons.lang3.StringUtils
    val stackTrace = Thread.currentThread.getStackTrace
    val parent = stackTrace(2)
    println(parent.getMethodName + ": " + StringUtils.join(arguments.toArray[Object], ", "))
  }

  private val processed = mutable.LinkedHashMap[String, XSNode]()

  def nodes: Seq[XSNode] = processed.values.toStream

  def process(obj: XSObject): Option[XSNode] = Option(obj)
    .flatMap { obj =>
      processed.get(XSNode.id(obj)) match {
        case opt@Some(_) => opt
        case None => {
          val node = new XSNode(obj)
          processed += (node.id -> node)
          processNewNode(node)
          Some(node)
        }
      }
    }

  private def process(list: XSObjectList): Seq[XSNode] = list
    .map { obj => process(obj.asInstanceOf[XSObject]) }
    .flatMap { obj => obj }

  private def processNewNode(@NotNull node: XSNode): Unit = {
    node.obj match {
      case o: XSElementDeclaration => process_element_declaration(node, o)
      case o: XSSimpleTypeDefinition => process_simple_type_definition(node, o)
      case o: XSComplexTypeDefinition => process_complex_type_definition(node, o)
      case o: XSModelGroup => process_model_group(node, o)
      case o: XSWildcard => process_wildcard(node, o)
      case o: XSParticle => throw new IllegalStateException("Particles should be processed separately.")
      case _ => throw new IllegalStateException(s"Unsupported object: ${node.obj.getClass.getSimpleName}.")
    }
  }

  private def process_element_declaration(node: XSNode, o: XSElementDeclaration): Unit = {
    $debug(node)
    process(o.getTypeDefinition).foreach { t => node.link(t.id, XSEdgeType.ElementType) }
    process(o.getSubstitutionGroupAffiliation).foreach { t => t.link(node.id, XSEdgeType.ElementSubstitution) } // reverse order
  }

  private def process_simple_type_definition(node: XSNode, o: XSSimpleTypeDefinition): Unit = {
    $debug(node)
    process(o.getBaseType).foreach { t => node.link(t.id, XSEdgeType.BaseType(XSConstants.DERIVATION_EXTENSION)) }
  }

  private def process_complex_type_definition(node: XSNode, o: XSComplexTypeDefinition): Unit = {
    $debug(node)
    process(o.getBaseType).foreach { t => node.link(t.id, XSEdgeType.BaseType(o.getDerivationMethod)) }
    process(o.getSimpleType).foreach { t => node.link(t.id, XSEdgeType.BaseType(XSConstants.DERIVATION_EXTENSION)) } // TODO: which derivation?

    process_particles(
      node,
      Option(o.getParticle)
    )
  }

  private def process_model_group(node: XSNode, o: XSModelGroup): Unit = {
    $debug(node)

    process_particles(
      node,
      o.getParticles.map { obj => obj.asInstanceOf[XSParticle] }
    )
  }

  private def process_wildcard(node: XSNode, o: XSWildcard): Unit = {
    $debug(node)
  }

  private def process_particles(node: XSNode, particles: Iterable[XSParticle]): Unit = {
    particles
    .foreach { p =>
      process(p.getTerm).foreach {
        t => node.link(t.id, XSEdgeType.Cardinality(p))
      }
    }
  }

}

