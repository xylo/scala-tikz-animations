package de.endrullis.sta

/**
 * A time interval.
 *
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
case class TimeInterval(start: Double, duration: Double = 0) {
	def extend(thatDuration: Double) = copy(duration = duration + thatDuration)
	def end = start + duration

	def contains (time: Double) = start <= time && time <= end

	def union(that: TimeInterval) = {
		val minStart = this.start min that.start
		val maxEnd   = this.end max that.end
		TimeInterval(minStart, maxEnd - minStart)
	}

	def intersect(that: TimeInterval): Option[TimeInterval] = {
		val minStart = this.start max that.start
		val maxEnd   = this.end min that.end

		if (minStart < maxEnd) {
			Some(TimeInterval(minStart, maxEnd - minStart))
		} else {
			None
		}
	}
}
