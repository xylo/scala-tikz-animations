package de.endrullis.sta

import org.specs2._

/** Tests for linear TimeMap.
	*
	* @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
	*/
class SinusTimeMapTest extends Specification with BaseVarIC { def is = s2"""

 A sinus TimeMap applied to a variable change from 0 to 1 should
   return  0.00 at 0.00      $checkTime0
   return ~0.15 at 0.25      $checkTime25
   return  0.50 at 0.50      $checkTime50
   return ~0.85 at 0.75      $checkTime75
   return  1.00 at 1.00      $checkTime100
 """

	implicit val timeMap = TimeMap.sin[Double]
	val pos = 0 changeTo 1 in 1

	def checkTime0 =
		pos.generate(0).get must beCloseTo(0d, 5.significantFigures)

	def checkTime25 =
		pos.generate(0.25).get must beCloseTo((Math.sin(3d/2d*Math.PI + 1d/4d*Math.PI) + 1) / 2, 5.significantFigures)

	def checkTime50 =
		pos.generate(0.50).get must beCloseTo(0.5d, 5.significantFigures)

	def checkTime75 =
		pos.generate(0.75).get must beCloseTo((Math.sin(3d/2d*Math.PI + 3d/4d*Math.PI) + 1) / 2, 5.significantFigures)

	def checkTime100 =
		pos.generate(1).get must beCloseTo(1d, 5.significantFigures)

}
