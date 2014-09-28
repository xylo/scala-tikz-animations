package de.endrullis.sta.generators

import de.endrullis.sta.{Generator, TimeMap}

/**
 * Rise up generator.
 *
 * @param f function that defines how to rise up.  It takes a start time, a duration, and a time map and returns a generator.
 * @tparam TM type of the time map
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
class RiseUp[TM](f: (Double, Double, TimeMap[TM]) => Generator[String]) {
	/** Lets the element appear immediately. */
	def nonAnimated = f(0, 0, TimeMap.linear[TM])
	/** Rises up the element over the given period of time. */
	def riseUpIn(duration: Int)(implicit timeMap: TimeMap[TM]) = f(0, duration, timeMap)
	/** Rises up the element over the given period of time. */
	def riseUpIn(timeMap: TimeMap[TM], duration: Double) = f(0, duration, timeMap)

	/** Starts this rise up in the given number of seconds. */
	def start(startTime: Double) = new {
		/** Lets the element appear immediately. */
		def nonAnimated = f(startTime, 0, TimeMap.linear[TM])
		/** Rises up the element over the given period of time. */
		def riseUpIn(duration: Int)(implicit timeMap: TimeMap[TM]) = f(startTime, duration, timeMap)
		/** Rises up the element over the given period of time. */
		def riseUpIn(timeMap: TimeMap[TM], duration: Double) = f(startTime, duration, timeMap)
	}
}
