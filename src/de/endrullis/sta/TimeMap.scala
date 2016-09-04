package de.endrullis.sta

import math._

/**
 * Function that maps a duration time (value between 0 and `duration`) to a new relative time (value between 0 and 1)
 * using the time mapping function `timeFun` which maps from a relative time to a relative time.
 *
 * @tparam T type for which the time map shall be used (only relevant for implicit casts)
 * @param timeFun time mapping from a relative time to a relative time
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
case class TimeMap[T](timeFun: TimeFun) {
	/** Returns a function that maps the duration time (value between 0 and `duration`) to a relative time (value between 0 and 1). */
	def apply(duration: Double): Double => Double = df => timeFun(df/duration)

	/** Returns a function that maps the duration time (value between 0 and `duration`) to the time mapped result of `f`. */
	def apply[A](duration: Double, f: Double => A): Double => A = apply(duration) andThen f

	def as[T2] = this.asInstanceOf[TimeMap[T2]]
}

object TimeMap {
	/** A linear time map (the identity function). */
	def linear[T] = TimeMap[T](TimeFun.linear)

	/** A sinus time map simulating acceleration and deceleration. */
	def sin[T] = TimeMap[T](TimeFun.sinus)

	/** A sinus time map simulating acceleration. */
	def accelerate[T] = TimeMap[T](TimeFun.accelerate)

	/** A sinus time map simulating deceleration. */
	def decelerate[T] = TimeMap[T](TimeFun.decelerate)
}

/**
 * Collection of relative time functions, i.e., functions that map relative time values to relative time values.
 */
object TimeFun {
	/** A linear time map (the identity function). */
	val linear: TimeFun = identity

	/** A sinus time map simulating acceleration and deceleration. */
	val sinus: TimeFun = rt => (math.cos(Pi + rt*Pi) + 1)/2

	/** A sinus time map simulating acceleration. */
	val accelerate: TimeFun = rt => 1-math.cos(rt*Pi/2)

	/** A sinus time map simulating deceleration. */
	val decelerate: TimeFun = rt => math.sin(rt*Pi/2)
}
