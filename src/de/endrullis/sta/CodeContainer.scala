package de.endrullis.sta

import collection.mutable

/**
 * Mutable container for code blocks.
 * Code blocks can be added using the `add` method.
 *
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
trait MutableCodeContainer extends CodeContainer {

	protected def timeIntervalJoin = (a,b) => Some(a union b)

	val codeGenerators = new mutable.MutableList[Generator[String]]

	def add(codeGenerator: => Generator[String]) {
		codeGenerators += codeGenerator
	}

	override def ~[B](that: Generator[B]) = {
		add(that.toStringGenerator)
		this
	}
}

class ImmutableCodeContainer(protected val timeIntervalJoin: (TimeInterval, TimeInterval) => Option[TimeInterval], val codeGenerators: Generator[String]*) extends CodeContainer

trait CodeContainer extends Generator[String] with AbstractContainer {

	override def toStringGenerator = this

	def generateInternal(time: Double) = codeGenerators.toList.map(_.generate(time)).filter(_.isDefined).map(_.get) match {
		case Nil  => None
		case list => Some(list.mkString)
	}

}

trait AbstractContainer {

	def codeGenerators: Iterable[Generator[_]]
	protected def timeIntervalJoin: (TimeInterval, TimeInterval) => Option[TimeInterval]

	def timeInterval = codeGenerators.toList.filter(_.timeInterval.isDefined).map(_.timeInterval.get) match {
		case Nil  => None
		case list => list.tail.foldLeft(Option(list.head))((a,b) => a.flatMap(timeIntervalJoin(_,b)))
	}
	def infiniteTimeInterval = if (timeInterval.isEmpty) None else {
		codeGenerators.toList.filter(_.infiniteTimeInterval.isDefined).map(_.infiniteTimeInterval.get) match {
			case Nil  => None
			case list => list.tail.foldLeft(Option(list.head))((a,b) => a.flatMap(timeIntervalJoin(_,b)))
		}
	}

	lazy val pauses = codeGenerators.flatMap(_.pauses).toSet

}
