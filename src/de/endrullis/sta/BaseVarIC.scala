package de.endrullis.sta

import java.awt.Color
import collection.mutable
import scala.language.implicitConversions

/**
 * Implicit casts for some base variable types.
 *
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
trait BaseVarIC extends PosIC {

	implicit def value2var[T,TM](value: T): Var[T,T] = Var[T](value)
	implicit def var2stringVar(value: String): StringVar[String] = StringVar[String](value)
	implicit def color2var(color: Color): ColorVar[Color] = ColorVar[Color](color)
	implicit def double2var(value: Double): DoubleVar[Double] = DoubleVar[Double](value)
	implicit def pos2var(pos: Pos): PosVar[Pos] = PosVar[Pos](pos)

	implicit class StringExt(s: String) {
		/**
		 * Injects the variables into the text.
		 *
		 * @param varMap maps variable names to the variables being injected.
		 * @return StringGenerator representing the text with injected variables
		 */
		def << (varMap: (String, Generator[Any])*): Generator[String] = {
			val end = "ềǹd"

			def split(s: String, name: String, v: Generator[String]): List[AnyRef] = {
				val y = ((s+end).split("\\$"+name).toList.foldLeft(List[AnyRef]())((a: List[AnyRef], b: String) => b :: v ::  a): @unchecked) match {
					case x :: xs => x.asInstanceOf[String].replaceAll(end+"$", "") :: xs
				}
				y.reverse.drop(1)
			}

			val list = varMap.toList.foldLeft(List[AnyRef](s)){ case (l: List[AnyRef], t: (String, Generator[Any])) =>
				l.flatMap{
					case s: String => split(s, t._1, t._2.toStringGenerator)
					case g: Generator[_] => List(g)
				}
			}

			new ImmutableCodeContainer(_ intersect _, list.map{
				case s: String => StringVar(s)
				case g: Generator[_] => g.asInstanceOf[Generator[String]]
			}: _*)
		}
	}

	implicit class GeneratorHelper(val sc: StringContext) {
	  def vi(args: Generator[Any]*): Generator[String] = {
		  val strings = sc.parts.iterator.map(_.stripMargin)
		  val gens = args.iterator

		  val list = new mutable.ListBuffer[AnyRef]
		  list += strings.next
		  while (strings.hasNext) {
			  list += gens.next
			  list += strings.next
		  }

		  if (list.head == "") list.remove(0)
		  if (list.last == "") list.remove(list.size-1)

		  new ImmutableCodeContainer(_ intersect _, (list: @unchecked).map{
				case s: String => StringVar(s)
				case g: Generator[Any] => g.toStringGenerator
			}: _*)
	  }
	}

	// arithmetic operations for double generators
	implicit class DoubleGeneratorExt(thisGen: Generator[Double]) {
		def + (thatGen: Generator[Double]) = (thisGen~~thatGen).map{case a~~b => a+b}
		def - (thatGen: Generator[Double]) = (thisGen~~thatGen).map{case a~~b => a-b}
		def * (thatGen: Generator[Double]) = (thisGen~~thatGen).map{case a~~b => a*b}
		def / (thatGen: Generator[Double]) = (thisGen~~thatGen).map{case a~~b => a/b}
	}
}
object BaseVarIC extends BaseVarIC
