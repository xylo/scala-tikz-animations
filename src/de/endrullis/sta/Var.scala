package de.endrullis.sta

/**
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
object Var {
	def apply[T](value: T) = new Var[T,Any](value)
}
case class Var[T, TM](state: VarState[T]) extends AbstractVar[T, TM, Var[T, TM]] {
	def this(value: T) = this(VarState(value))

	protected def copyWithState(state: VarState[T]) = copy(state)
}

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
	lazy val infiniteTimeInterval: Option[TimeInterval] = infiniteEvents.toList match {
		case Nil => None
		case _=> Some(TimeInterval(infiniteEvents.head.startTime, infiniteEvents.last.endTime - infiniteEvents.head.startTime))
	}

	def generateInternal(time: Double): Option[T] = {
		infiniteEvents.find(_.contains(time)).map(_.generate(time))
	}

	def pause() = copyWithState(state.copy(pauses = state.pauses + state.nextStartTime))

	def pauses = state.pauses

	def start(time: Double) = copyWithState(state.copy(nextStartTime = time))
	def startWith(timeInterval: TimeInterval) = start(timeInterval.start)
	def startAfter(timeInterval: TimeInterval) = start(timeInterval.end)

	def setTo(value: T): TL = copyWithState(state.copy(lastValue = value))

	def stay(duration: Double): TL = perform(_ => state.lastValue, duration)(TimeMap.linear)

	def change(valueGen: Double => T) = new {
		def in(duration: Double)(implicit timeMap: TimeMap[TM]): TL = perform(valueGen, duration)
		def in(timeMap: TimeMap[TM], duration: Double): TL = perform(valueGen, duration)(timeMap)
	}

	def hide(duration: Double): TL = copyWithState(state.copy(nextStartTime = state.nextStartTime + duration))

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

	def createEvent(duration: Double, valueGen: Double => T) = {
		TimeSlot(state.nextStartTime, duration, valueGen)
	}
}

trait Generator[+T] { thisGenerator =>
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

	/** Concatenates this and the given generator. */
	def ~[B] (that: Generator[B]): CodeContainer = new ImmutableCodeContainer (_ intersect _,
		thisGenerator.toStringGenerator,
		that.toStringGenerator
	)

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

case class TimeSlot[T](startTime: Double, duration: Double, time2value: Double => T) {
	def endTime = startTime + duration

	def contains(time: Double) = startTime <= time && time < endTime

	def generate(time: Double) = time2value(time - startTime)
}
class EndTimeSlot[T](time: Double) extends TimeSlot[T](time, 0, null) {
}
