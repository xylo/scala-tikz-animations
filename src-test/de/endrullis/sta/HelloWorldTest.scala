package de.endrullis.sta

import org.specs2._
import org.specs2.specification.BeforeAll

/**
	* Tests for a small 'Hello World' example.
	*
	* @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
	*/
class HelloWorldTest extends Specification with BeforeAll with BaseVarIC { def is = s2"""

 This 'Hello World' animation should
   have 1 frame without last frame    $frameCountWithoutLastFrame
   have 2 frames with last frame      $frameCountWithLastFrame
   contain the frame "Hello World"    $containFrameHelloWorld
 """

	object Ani extends ScalaTikzAni {
		add("Hello World" start 0 stay 1)
	}

	def beforeAll = Ani.main(Array())


	def frameCountWithoutLastFrame =
		Ani.frames(true).size must_== 2

	def frameCountWithLastFrame =
		Ani.frames(false).size must_== 1

	def containFrameHelloWorld =
		Ani.frames(false) must_== List("Hello World")

}
