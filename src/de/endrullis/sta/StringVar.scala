package de.endrullis.sta


/**
 * String variable.
 *
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
object StringVar {
	def apply[TM](startValue: String) = new StringVar[TM](startValue, VarState(startValue))
}
case class StringVar[TM](startValue: String, state: VarState[String]) extends AbstractVar[String, TM, StringVar[TM]] { thisGenerator =>
	protected def copyWithState(state: VarState[String]) = copy(state = state)

	override def ~[B](that: Generator[B]) = new ImmutableCodeContainer(_ intersect _,
		thisGenerator,
		that.toStringGenerator
	)
}
