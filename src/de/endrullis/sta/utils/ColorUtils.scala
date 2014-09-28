package de.endrullis.sta.utils

import java.awt.Color

/**
 * Collection of color utility functions.
 *
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
object ColorUtils {
	def fadeIn(): Double => String = rt => "opacity="+rt

	def fadeOut(): Double => String = rt => "opacity="+(1-rt)

	def fadeOver(color1: String, color2: String): Double => String = {
		rt => color1 + "!" + (100-rt*100) + "!" + color2
	}

	def fadeOver(color1: Color, color2: Color): Double => Color = {
		rt => new Color(
			(color1.getRed   + (color2.getRed   - color1.getRed  )*rt).round.toInt,
			(color1.getGreen + (color2.getGreen - color1.getGreen)*rt).round.toInt,
			(color1.getBlue  + (color2.getBlue  - color1.getBlue )*rt).round.toInt
		)
	}
}
