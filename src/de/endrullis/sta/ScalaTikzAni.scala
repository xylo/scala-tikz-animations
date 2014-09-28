package de.endrullis.sta

import java.awt.Color

/**
 * Default class for creating Tikz animations.
 * It extends the BaseTikzAni by functions of BaseVarIC.
 * Inherit from this class to describe your Tikz animation.
 *
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
class ScalaTikzAni extends BaseTikzAni with BaseVarIC {

	def defColor(name: String, definition: Generator[Color]) = ("""\definecolor{"""+name+"}{RGB}{")~definition~"}\n"
	def defTikzStyle(name: String, definition: Generator[String]) = ("""\tikzstyle{"""+name+"} = [")~definition~"]\n"
	def scope(options: Generator[String], code: Generator[String]) = """\begin{scope}["""~options~"]"~code~"""\end{scope}"""
	def shift(code: Generator[String], pos: Generator[Pos]) = scope("shift={"~pos~"}", code)

}
