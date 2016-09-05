package de.endrullis.sta

import org.junit.runner.RunWith
import org.specs2._
import org.specs2.runner.JUnitRunner

/**
	* Tests for linear TimeMap.
	*
	* @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
	*/
@RunWith(classOf[JUnitRunner])
class LinearTimeMapSpec extends Specification with BaseVarIC { def is = s2"""

 A linear TimeMap applied to a variable change from 0 to 1 should
   return 0.00 at 0.00      $checkTime0
   return 0.25 at 0.25      $checkTime25
   return 0.50 at 0.50      $checkTime50
   return 0.75 at 0.75      $checkTime75
   return 1.00 at 1.00      $checkTime100
 """

	implicit val timeMap = TimeMap.linear[Double]
	val pos = 0 changeTo 1 in 1

	def checkTime0 =
		pos.generate(0) must_== Some(0)

	def checkTime25 =
		pos.generate(0.25) must_== Some(0.25)

	def checkTime50 =
		pos.generate(0.50) must_== Some(0.50)

	def checkTime75 =
		pos.generate(0.75) must_== Some(0.75)

	def checkTime100 =
		pos.generate(1) must_== Some(1)

}
