package de.endrullis

import scala.language.implicitConversions

/**
 * Package object for sta.
 *
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
package object sta {

	type TimeFun = Double => Double

	implicit class doubleTildeExt[T1](t1: T1) {
		def ~~[T2](t2: T2): T1 ~~ T2 = new ~~(t1, t2)
	}

}
