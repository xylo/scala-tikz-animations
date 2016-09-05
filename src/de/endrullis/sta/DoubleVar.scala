package de.endrullis.sta

/**
	* Double variable.
	*
	* @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
	*/
object DoubleVar {
	def apply[TM](startValue: Double): DoubleVar[TM] = new DoubleVar[TM](startValue, VarState(startValue))
}
case class DoubleVar[TM](startValue: Double, state: VarState[Double]) extends AbstractVar[Double, TM, DoubleVar[TM]] {
	protected def copyWithState(state: VarState[Double]) = copy(state = state)

	def changeTo(value2: Double) = new ChangeTo(value2)
	class ChangeTo(value2: Double) {
		def in(duration: Double)(implicit timeMap: TimeMap[TM]) = change(rt => state.lastValue + (value2 - state.lastValue)*rt).in(duration)
		def in(timeMap: TimeMap[TM], duration: Double) = change(rt => state.lastValue + (value2 - state.lastValue)*rt).in(duration)(timeMap)
	}

	override def toStringGenerator = map(Pos.numberFormat.format)
}
