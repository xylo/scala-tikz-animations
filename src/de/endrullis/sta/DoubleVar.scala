package de.endrullis.sta


/**
 * Double variable.
 *
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
case class DoubleVar[TM](startValue: Double, state: VarState[Double]) extends AbstractVar[Double, TM, DoubleVar[TM]] {
	protected def copyWithState(state: VarState[Double]) = copy(state = state)

	def changeTo(value2: Double) = new {
		def in(duration: Double)(implicit timeMap: TimeMap[TM]) = change(rt => state.lastValue + (value2 - state.lastValue)*rt).in(duration)
		def in(timeMap: TimeMap[TM], duration: Double) = change(rt => state.lastValue + (value2 - state.lastValue)*rt).in(duration)(timeMap)
	}

	override def toStringGenerator = map(Pos.numberFormat.format)
}
