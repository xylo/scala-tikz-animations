package de.endrullis.sta

import scala.language.implicitConversions

/**
 * Implicit casts for positions ([[de.endrullis.sta.Pos]]).
 *
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
trait PosIC {
	implicit def doublePair2pos(pos: (Double, Double)): Pos = Pos(pos._1, pos._2)
	implicit class double2posCompatibleDouble(d: Double) {
		def * (pos: Pos) = pos * d
	}
}
