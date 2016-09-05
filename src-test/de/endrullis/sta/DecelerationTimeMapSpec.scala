package de.endrullis.sta

import java.lang.Math._

import org.junit.runner.RunWith
import org.specs2._
import org.specs2.runner.JUnitRunner

/**
	* Tests for deceleration TimeMap.
	*
	* @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
	*/
@RunWith(classOf[JUnitRunner])
class DecelerationTimeMapSpec extends Specification with BaseVarIC { def is = s2"""

 A deceleration TimeMap applied to a variable change from 0 to 1 should
   return  0.00 at 0.00      $checkTime0
   return ~0.38 at 0.25      $checkTime25
   return ~0.71 at 0.50      $checkTime50
   return ~0.92 at 0.75      $checkTime75
   return  1.00 at 1.00      $checkTime100
 """

	implicit val timeMap = TimeMap.decelerate[Double]
	val pos = 0 changeTo 1 in 1

	def checkTime0 =
		pos.generate(0).get must beCloseTo(0d, 5.significantFigures)

	def checkTime25 =
		pos.generate(0.25).get must beCloseTo(sin(1d/8 * PI), 5.significantFigures)

	def checkTime50 =
		pos.generate(0.50).get must beCloseTo(sin(1d/4 * PI), 5.significantFigures)

	def checkTime75 =
		pos.generate(0.75).get must beCloseTo(sin(3d/8 * PI), 5.significantFigures)

	def checkTime100 =
		pos.generate(1).get must beCloseTo(1d, 5.significantFigures)

}
