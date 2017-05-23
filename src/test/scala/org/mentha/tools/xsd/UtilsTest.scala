package org.mentha.tools.xsd

import org.apache.commons.lang3.StringUtils
import org.scalatest._

/** */
class UtilsTest extends FlatSpec with Matchers {

  private def testFlatExtend(values: String*) = {
    def id(el: String): Int = el.hashCode
    Utils.flatExtend[String, Int](
      stream = Stream(values:_*),
      visited = Set(),
      extend = (el, v) => Option(StringUtils.trimToNull(el))
        .map { el => StringUtils.trimToNull(el.substring(1)) }
        .filter { el => !v.contains(id(el)) }
        .toSeq,
      id = id
    )
  }

  behavior of "flatExtend method"

  it should "add next level to both `result` and `visited`" in {
    val (n, v) = testFlatExtend("00", "01", "02")
    v should contain theSameElementsAs {
      Seq("0", "1", "2") map { x => x.hashCode }
    }
    n should contain theSameElementsAs {
      Seq("0", "1", "2")
    }
  }

  it should "keep the original stream order for flat source" in {
    val (n, _) = testFlatExtend("00", "01", "02")
    n should contain inOrder("0", "1", "2")
  }

  it should "keep the original stream order for repetitive source" in {
    val (n, _) = testFlatExtend("00", "01", "10", "20")
    n should contain inOrder("0", "1")
  }


  behavior of "filter method"

  it should "keep" in {

  }



}
