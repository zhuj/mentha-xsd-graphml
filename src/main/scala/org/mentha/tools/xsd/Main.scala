package org.mentha.tools.xsd

import java.io.File
import java.nio.charset.Charset

import org.apache.commons.io.FileUtils
import org.apache.xerces.xs._
import collection.convert.wrapAsScala._

/** */
object Main {

  def main(args: Array[String]): Unit = {

    val source = args(0)
    val namespace = if (args.length > 1) args(1) else null

    val model = XSDParser.loadModel(source)
    val processor = new XSDProcessor(model)

    val components = model.getComponentsByNamespace(XSConstants.ELEMENT_DECLARATION, namespace)
    components.values()
      .map { obj => obj.asInstanceOf[XSElementDeclaration] }
      .foreach { obj =>
        processor.process(obj)
      }

    var nodes: Seq[XSNode] = processor.nodes

    // TODO: filter
    // val nodes = processor.nodes
    //   .filterNot { n => "http://www.w3.org/1998/Math/MathML" == n.obj.getNamespace}

    nodes = XSDPostProcessor.clearRestrictAnyType(nodes)
    nodes = XSDPostProcessor.simplifyModelGroups(nodes)

    val graphMl = GraphMLRenderer.render(nodes)

    FileUtils.write(
      new File(source+".graphml"),
      graphMl.toString,
      Charset.forName("UTF-8")
    )
  }

}
