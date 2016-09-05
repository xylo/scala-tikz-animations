package de.endrullis.sta.generators

import de.endrullis.sta.{Generator, TimeMap}

/**
 * Fade in generator.
 *
 * @param f function that defines how to fade in.  It takes a start time, a duration, and a time map and returns a generator.
 * @tparam TM type of the time map
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
class FadeIn[TM](f: (Double, Double, TimeMap[TM]) => Generator[String]) {
	/** Lets the element appear immediately. */
	def nonAnimated = f(0, 0, TimeMap.linear[TM])
	/** Fades in the element over the given period of time. */
	def fadeInIn(duration: Int)(implicit timeMap: TimeMap[TM]) = f(0, duration, timeMap)
	/** Fades in the element over the given period of time. */
	def fadeInIn(timeMap: TimeMap[TM], duration: Double) = f(0, duration, timeMap)

	/** Starts this fade in in the given number of seconds. */
	def start(startTime: Double) = new Start(startTime)
	class Start(startTime: Double) {
		/** Lets the element appear immediately. */
		def nonAnimated = f(startTime, 0, TimeMap.linear[TM])
		/** Fades in the element over the given period of time. */
		def fadeInIn(duration: Int)(implicit timeMap: TimeMap[TM]) = f(startTime, duration, timeMap)
		/** Fades in the element over the given period of time. */
		def fadeInIn(timeMap: TimeMap[TM], duration: Double) = f(startTime, duration, timeMap)
	}
}
