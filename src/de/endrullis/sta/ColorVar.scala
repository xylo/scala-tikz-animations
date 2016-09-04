package de.endrullis.sta

import java.awt.Color
import utils.ColorUtils

/**
	* Color variable.
	*
	* @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
	*/
object ColorVar {
	def apply[TM](startColor: Color): ColorVar[TM] = new ColorVar[TM](startColor, VarState(startColor))
}
case class ColorVar[TM](startColor: Color, state: VarState[Color]) extends AbstractVar[Color, TM, ColorVar[TM]] {
	protected def copyWithState(state: VarState[Color]) = copy(state = state)

	/** Cross fades to the given color. */
	def changeTo(color2: Color) = new {
		/** In the given duration. */
		def in(duration: Double)(implicit timeMap: TimeMap[TM]) = change(ColorUtils.fadeOver(state.lastValue, color2)).in(duration)
		/** In the given duration. */
		def in(timeMap: TimeMap[TM], duration: Double) = change(ColorUtils.fadeOver(state.lastValue, color2)).in(duration)(timeMap)
	}

	override def toStringGenerator = map(c => c.getRed+","+c.getGreen+","+c.getBlue)
}
