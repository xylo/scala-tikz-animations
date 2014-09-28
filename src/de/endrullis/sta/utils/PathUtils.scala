package de.endrullis.sta.utils

import de.endrullis.sta.{PosIC, Pos, TimeMap}

/**
 * Collection of path utility functions.
 *
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
object PathUtils extends PosIC {
	/** Returns a movement function from `pos1` to `pos2`. */
	def straightMovement[T](pos1: Pos, pos2: Pos, duration: Double)(implicit timeMap: TimeMap[T]): Double => Pos = {
		timeMap(duration, straightMovement(pos1, pos2))
	}

	/** Returns a time normalized movement function from `pos1` to `pos2`, i.e., a function that takes a relative time and returns a position. */
	def straightMovement(pos1: Pos, pos2: Pos): Double => Pos = {
		val vector = pos2 - pos1
		(time: Double) => pos1 + vector*time
	}
}
