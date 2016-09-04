package de.endrullis.sta

import org.specs2._
import org.specs2.specification.BeforeAll

/**
	* Tests for PosVar.
	*
	* @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
	*/
class PosVarTest extends Specification with BeforeAll with BaseVarIC { def is = s2"""

 This 'PosVarTest' animation should
   have 3 frames including the last frame      $frameCount
   contain the frames (0,2), (0.5,3), (1,4)    $containFrameHelloWorld
 """

	object Ani extends ScalaTikzAni {
		implicit val movementTimeMap = TimeMap.linear[Pos]

		val pos = Pos(0, 2) changeTo Pos(1, 4) in 2

		add(vi"$pos")
	}

	def beforeAll = Ani.main(Array())


	def frameCount =
		Ani.frames(true).size must_== 3

	def containFrameHelloWorld =
		Ani.frames(true) must_== List("(0,2)", "(0.5,3)", "(1,4)")

}
