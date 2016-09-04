package de.endrullis.sta

import utils.PathUtils

/**
	* A position variable for Tikz.
	*
	* @param startPos start position
	* @param state    variable state
	* @tparam TM      type of the TimeMap
	* @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
	*/
case class PosVar[TM](startPos: Pos, state: VarState[Pos]) extends AbstractVar[Pos, TM, PosVar[TM]] { thisVar =>
	protected def copyWithState(state: VarState[Pos]) = copy(state = state)

	def changeTo(color2: Pos) = new {
		def in(duration: Double)(implicit timeMap: TimeMap[TM]) = change(PathUtils.straightMovement(state.lastValue, color2)).in(duration)
		def in(timeMap: TimeMap[TM], duration: Double) = change(PathUtils.straightMovement(state.lastValue, color2)).in(duration)(timeMap)
	}

	def + (thatVar: Generator[Pos]) = new Generator[Pos] {
		def timeInterval = combine(thisVar.timeInterval, thatVar.timeInterval)(_ union _)
		def infiniteTimeInterval = combine(thisVar.infiniteTimeInterval, thatVar.infiniteTimeInterval)(_ union _)
		def generateInternal(time: Double) = combine(thisVar.generate(time), thatVar.generate(time))(_ + _)
		def pauses = thisVar.pauses union thatVar.pauses
	}

	/*
	def union(timeIntervals: Option[TimeInterval]*): Option[TimeInterval] = {
		timeIntervals.filter(_ != None).toList match {
			case Nil => None
			case xs => Some(xs.map(_.get).reduce(_ union _))
		}
	}
	*/

	protected def combine[T](o1: Option[T], o2: Option[T])(f: (T, T) => T): Option[T] = for (v1 <- o1; v2 <- o2) yield f(v1,v2)

	//override def toStringGenerator = map(c => c.getRed+","+c.getGreen+","+c.getBlue)
}
