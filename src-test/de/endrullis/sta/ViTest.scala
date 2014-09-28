package de.endrullis.sta

import collection.mutable

/**
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
object ViTest extends App {

	implicit class TestHelper(val sc: StringContext) {
	  def vi(args: Any*): String = {
		  val strings = sc.parts.iterator.map(_.stripMargin)
		  val gens = args.iterator

		  val list = new mutable.ListBuffer[AnyRef]
		  list += strings.next
		  while (strings.hasNext) {
			  list += gens.next().toString
			  list += strings.next
		  }

		  if (list.head == "") list.remove(0)
		  if (list.last == "") list.remove(list.size-1)

		  list.mkString
	  }
	}

	val x = "test"
	val y = vi"""a $x
	            |b"""

	println(y)

}
