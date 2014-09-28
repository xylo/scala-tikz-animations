package de.endrullis.sta

import java.text.NumberFormat
import java.util.Locale
import Pos._

/**
 * A 2D position.
 *
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
case class Pos(x: Double, y: Double) {
	def + (that: Pos) = Pos(this.x + that.x, this.y + that.y)
	def - (that: Pos) = Pos(this.x - that.x, this.y - that.y)
	def * (d: Double) = Pos(this.x * d, this.y * d)
	def / (d: Double) = Pos(this.x / d, this.y / d)

	override def toString = "("+numberFormat.format(x)+","+numberFormat.format(y)+")"
}
object Pos {
	val numberFormat = NumberFormat.getInstance(Locale.ENGLISH)

	def apply(xVar: Generator[Double], yVar: Generator[Double]): Generator[Pos] = (xVar~~yVar).map{case x~~y => Pos(x,y)}
}
