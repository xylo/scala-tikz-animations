package de.endrullis.sta

/**
	* A timed variable.
	*
	* @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
  */
object Var {
	def apply[T](value: T) = new Var[T,T](value)
}
/**
	* A timed variable of type `T` with requires a TimeMap for `TM`.
	*
	* @param state variable state
	* @tparam T    type of the variable
	* @tparam TM   type of the TimeMap
	*/
case class Var[T, TM](state: VarState[T]) extends AbstractVar[T, TM, Var[T, TM]] {
	def this(value: T) = this(VarState(value))

	protected def copyWithState(state: VarState[T]) = copy(state)
}

/**
	* A state of a timed variable.
	*
	* @param lastValue      the last value of the variable
	* @param nextStartTime  start time of the next time event
	* @param eventsRev      reverse list of the events of this variable collected so far
	* @param pauses         set of frames this variable shall be paused
	* @tparam T             type of the variable
	*/
case class VarState[T](lastValue: T, nextStartTime: Double = 0, eventsRev: List[TimeSlot[T]] = Nil, pauses: Set[Double] = Set())

abstract class AbstractVar[T, TM, TL <: AbstractVar[T, TM, TL]] extends Generator[T] {
	def state: VarState[T]

	lazy val events = state.eventsRev.reverse
	lazy val infiniteEvents = events.lastOption match {
		case None => List(infiniteEvent)
		case Some(last) => last match {
			case _: EndTimeSlot[T] => events
			case _ => events ::: List(infiniteEvent)
		}
	}

	def infiniteEvent = createEvent(Int.MaxValue - state.nextStartTime, _ => state.lastValue)

	lazy val timeInterval: Option[TimeInterval] = events match {
		case Nil => None
		case _ => Some(TimeInterval(events.head.startTime, state.eventsRev.head.endTime - events.head.startTime))
	}
	lazy val infiniteTimeInterval: Option[TimeInterval] = infiniteEvents match {
		case Nil => None
		case _=> Some(TimeInterval(infiniteEvents.head.startTime, infiniteEvents.last.endTime - infiniteEvents.head.startTime))
	}

	def generateInternal(time: Double): Option[T] = {
		infiniteEvents.find(_.contains(time)).map(_.generate(time))
	}

	/** Pauses the content for the next frame. */
	def pause() = copyWithState(state.copy(pauses = state.pauses + state.nextStartTime))

	/** Returns all pause times. */
	def pauses = state.pauses

	/** Defines that the content starts appearing after the given amount of time. */
	def start(time: Double) = copyWithState(state.copy(nextStartTime = time))
	/** Defines that the content starts appearing with the given time interval. */
	def startWith(timeInterval: TimeInterval) = start(timeInterval.start)
	/** Defines when the content starts appearing after the given time interval. */
	def startAfter(timeInterval: TimeInterval) = start(timeInterval.end)

	/** Sets the content to a new value. */
	def setTo(value: T): TL = copyWithState(state.copy(lastValue = value))

	/** Lets the content stay for the given amount of time. */
	def stay(duration: Double): TL = perform(_ => state.lastValue, duration)(TimeMap.linear)

	/** Lets the content change over time using the given value generator and duration. */
	def change(valueGen: Double => T) = new {
		def in(duration: Double)(implicit timeMap: TimeMap[TM]): TL = perform(valueGen, duration)
		def in(timeMap: TimeMap[TM], duration: Double): TL = perform(valueGen, duration)(timeMap)
	}

	/** Hides the content for the given amount of time. */
	def hide(duration: Double): TL = copyWithState(state.copy(nextStartTime = state.nextStartTime + duration))

	/** Lets the content disappear after the next frame. */
	def stop: TL = copyWithState(
		state.copy(
			eventsRev = new EndTimeSlot[T](state.nextStartTime) :: state.eventsRev
		)
	)

	protected def perform(valueGen: Double => T, duration: Double)(implicit timeMap: TimeMap[TM]): TL = {
		copyWithState(nextState(valueGen, duration))
	}

	protected def copyWithState(state: VarState[T]): TL

	protected def nextState(valueGen: Double => T, duration: Double)(implicit timeMap: TimeMap[TM]) = {
		val tmValueGen = timeMap(duration, valueGen)
		state.copy(
			lastValue = tmValueGen(duration),
			nextStartTime = state.nextStartTime + duration,
			eventsRev = createEvent(duration, tmValueGen) :: state.eventsRev
		)
	}

	protected def createEvent(duration: Double, valueGen: Double => T) = {
		TimeSlot(state.nextStartTime, duration, valueGen)
	}
}

/** A generator is an abstract unit text/code depending on . */
trait Generator[+T] { thisGenerator =>
	/** Returns the time interval this generator is defined for. */
	def timeInterval: Option[TimeInterval]
	def infiniteTimeInterval: Option[TimeInterval]

	def pauses: Set[Double]

	/** Returns the frame for the given point in time or None if no frame can be generated. */
	def generate(time: Double): Option[T] = {
		infiniteTimeInterval.flatMap{ interval =>
			if (interval contains time) {
				generateInternal(time)
			} else {
				None
			}
		}
	}
	/** Called by generate to generate a frame for the given time if the . */
	def generateInternal(time: Double): Option[T]

	/** Converts this generator into a string generator. */
	def toStringGenerator = map{
		case d: Double => Pos.numberFormat.format(d)
		case s => s.toString
	}

	/** Maps this generator to a new generator using mapping function `f`. */
	def map[B] (f: T => B) = new Generator[B] {
		def timeInterval = thisGenerator.timeInterval
		def infiniteTimeInterval = thisGenerator.infiniteTimeInterval
		def generateInternal(time: Double) = thisGenerator.generate(time).map(f)
		def pauses = thisGenerator.pauses
	}

	/** Concatenates this and that generator.
		* The resulting generator is defined on the intersection of the time intervals of both generators.
		*/
	def ~[B] (that: Generator[B]): CodeContainer = new ImmutableCodeContainer (_ intersect _,
		thisGenerator.toStringGenerator,
		that.toStringGenerator
	)

	/** Combines this and that generator to a generator returning both results as a ~~ pair.
		* The resulting generator is defined on the intersection of the time intervals of both generators.
		*/
	def ~~[B] (that: Generator[B]) = new Generator[T~~B] with AbstractContainer {
		def codeGenerators = List(thisGenerator, that)

		protected def timeIntervalJoin = _ intersect _

		def generateInternal(time: Double): Option[T~~B] = (thisGenerator.generate(time), that.generate(time)) match {
			case (Some(a), Some(b)) => Some(new ~~(a,b))
			case _ => None
		}
	}
}

case class ~~[+A,+B](_1: A, _2: B)

/**
	* A TimeSlot defines a value generation function for a certain time slot.
	*
	* @param startTime  time when the value generation shall start
	* @param duration   duration of the value generation
	* @param time2value value generation function that takes the time as input
	* @tparam T         type of the generated value
	*/
case class TimeSlot[T](startTime: Double, duration: Double, time2value: Double => T) {
	def endTime = startTime + duration

	def contains(time: Double) = startTime <= time && time < endTime

	def generate(time: Double) = time2value(time - startTime)
}
/** The EndTimeSlot marks the last frame of a timed variable. */
class EndTimeSlot[T](time: Double) extends TimeSlot[T](time, 0, null) {
}
