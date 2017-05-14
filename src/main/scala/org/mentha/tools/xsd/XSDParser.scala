package org.mentha.tools.xsd

import org.apache.xerces.impl.xs.XSImplementationImpl
import org.w3c.dom.bootstrap.DOMImplementationRegistry

/** */
object XSDParser {

  @throws[ReflectiveOperationException]
  def loadModel(uri: String) = {
    val registry = DOMImplementationRegistry.newInstance
    val impl = registry.getDOMImplementation("XS-Loader").asInstanceOf[XSImplementationImpl]
    val schemaLoader = impl.createXSLoader(null)
    schemaLoader.loadURI(uri)
  }

}
