package org.mentha.tools.xsd

import java.io.File
import java.nio.charset.Charset

import org.apache.commons.io.FileUtils

/** */
object Main {

  def main(args: Array[String]): Unit = {

    val source = args(0)
    val namespace = args(1)
    val element = args(2)

    val model = XSDParser.loadModel(source)
    val root = model.getElementDeclaration(element, namespace)
    val processor = new XSDProcessor(model)
    processor.process(root)

    var nodes: Seq[XSNode] = processor.nodes

    // TODO: filter
    // val nodes = processor.nodes
    //   .filterNot { n => "http://www.w3.org/1998/Math/MathML" == n.obj.getNamespace}


    nodes = XSDPostProcessor.simplifyModelGroups(nodes)

    val graphMl = GraphMLRenderer.render(nodes)

    FileUtils.write(
      new File(source+".graphml"),
      graphMl.toString,
      Charset.forName("UTF-8")
    )
  }

}
