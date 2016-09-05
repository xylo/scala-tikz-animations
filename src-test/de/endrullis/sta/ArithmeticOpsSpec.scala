package de.endrullis.sta

import org.junit.runner.RunWith
import org.specs2._
import org.specs2.runner.JUnitRunner

/**
	* Tests for arithmetic variable operations.
	*
	* @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
	*/
@RunWith(classOf[JUnitRunner])
class ArithmeticOpsSpec extends Specification with BaseVarIC { def is = s2"""

 x[0 -> 1 in 1s] + y[2 -> 3 in 2s] should
   have a time interval of 1s duration         $plusTimeIntervalDuration
   return 2.00 at 0.0s                         $plus0
   return 2.75 at 0.5s                         $plus50
   return 3.50 at 1.0s                         $plus100

 x[0 -> 1 in 1s] - y[2 -> 3 in 2s] should
   return -2.00 at 0.0s                        $minus0
   return -1.75 at 0.5s                        $minus50
   return -1.50 at 1.0s                        $minus100

 x[0 -> 1 in 1s] * y[2 -> 3 in 2s] should
   return 0        at 0.0s                     $mul0
   return 0.5*2.25 at 0.5s                     $mul50
   return 1.0*2.50 at 1.0s                     $mul100

 x[0 -> 1 in 1s] / y[2 -> 3 in 2s] should
   return 0        at 0.0s                     $div0
   return 0.5/2.25 at 0.5s                     $div50
   return 1.0/2.50 at 1.0s                     $div100
 """

	implicit val timeMap = TimeMap.linear[Double]

	val x = 0 changeTo 1 in 1
	val y = 2 changeTo 3 in 2

	val plus  = x + y
	val minus = x - y
	val mul   = x * y
	val div   = x / y

	def plusTimeIntervalDuration =
		plus.timeInterval.get.duration must_== 1

	def plus0 =
		plus.generate(0.00).get must beCloseTo(2.00, 5.significantFigures)

	def plus50 =
		plus.generate(0.50).get must beCloseTo(2.75, 5.significantFigures)

	def plus100 =
		plus.generate(1.00).get must beCloseTo(3.50, 5.significantFigures)

	
	def minus0 =
		minus.generate(0.00).get must beCloseTo(-2.00, 5.significantFigures)

	def minus50 =
		minus.generate(0.50).get must beCloseTo(-1.75, 5.significantFigures)

	def minus100 =
		minus.generate(1.00).get must beCloseTo(-1.50, 5.significantFigures)

	
	def mul0 =
		mul.generate(0.00).get must beCloseTo(0.0, 5.significantFigures)

	def mul50 =
		mul.generate(0.50).get must beCloseTo(0.5 * 2.25, 5.significantFigures)

	def mul100 =
		mul.generate(1.00).get must beCloseTo(1.0 * 2.5, 5.significantFigures)


	def div0 =
		div.generate(0.00).get must beCloseTo(0.0, 5.significantFigures)

	def div50 =
		div.generate(0.50).get must beCloseTo(0.5 / 2.25, 5.significantFigures)

	def div100 =
		div.generate(1.00).get must beCloseTo(1.0 / 2.5, 5.significantFigures)

}
