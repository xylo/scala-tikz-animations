package de.endrullis.sta

import org.specs2._
import Math._

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

/**
	* Tests for acceleration TimeMap.
	*
	* @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
	*/
@RunWith(classOf[JUnitRunner])
class AccelerationTimeMapSpec extends Specification with BaseVarIC { def is = s2"""

 An acceleration TimeMap applied to a variable change from 0 to 1 should
   return  0.00 at 0.00      $checkTime0
   return ~0.08 at 0.25      $checkTime25
   return ~0.29 at 0.50      $checkTime50
   return ~0.62 at 0.75      $checkTime75
   return  1.00 at 1.00      $checkTime100
 """

	implicit val timeMap = TimeMap.accelerate[Double]
	val pos = 0 changeTo 1 in 1

	def checkTime0 =
		pos.generate(0).get must beCloseTo(0d, 5.significantFigures)

	def checkTime25 =
		pos.generate(0.25).get must beCloseTo(1 - cos(1d/8 * PI), 5.significantFigures)

	def checkTime50 =
		pos.generate(0.50).get must beCloseTo(1 - cos(1d/4 * PI), 5.significantFigures)

	def checkTime75 =
		pos.generate(0.75).get must beCloseTo(1 - cos(3d/8 * PI), 5.significantFigures)

	def checkTime100 =
		pos.generate(1).get must beCloseTo(1d, 5.significantFigures)

}
