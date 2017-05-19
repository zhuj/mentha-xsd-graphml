package org.mentha.tools.xsd

import scala.annotation.tailrec

/**  */
object Utils {

  // TODO: optimize me: do not (re)invent the wheel
  def collect[E, ID](stream: Stream[E], visited: Set[ID], extend: (E, Set[ID]) => Seq[E], id: E => ID) = {
    @tailrec def core(stream: Stream[E], visited: Set[ID], result: Stream[E]): (Stream[E], Set[ID]) = stream match {
      case Stream.Empty => (result, visited)
      case el #:: tail => {
        val extension = extend(el, visited)
        core(
          stream = tail,
          visited = visited ++ extension.map { id },
          result = result #::: extension.toStream
        )
      }
    }
    core(
      stream = stream,
      visited = visited,
      result = Stream.Empty
    )
  }

  // TODO: optimize me: do not (re)invent the wheel
  def filter[E, ID](stream: Stream[E], extend: (E, Set[ID]) => Seq[E], id: E => ID) = collect[E, ID](
    stream = stream,
    visited = Set[ID](),
    extend = extend,
    id = id
  )._1

}
