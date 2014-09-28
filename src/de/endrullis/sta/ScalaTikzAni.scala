package de.endrullis.sta

import java.awt.Color

/**
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
class ScalaTikzAni extends BaseTikzAni with BaseVarIC {

	def defColor(name: String, definition: Generator[Color]) = ("""\definecolor{"""+name+"}{RGB}{")~definition~"}\n"
	def defTikzStyle(name: String, definition: Generator[String]) = ("""\tikzstyle{"""+name+"} = [")~definition~"]\n"
	def scope(options: Generator[String], code: Generator[String]) = """\begin{scope}["""~options~"]"~code~"""\end{scope}"""
	def shift(code: Generator[String], pos: Generator[Pos]) = scope("shift={"~pos~"}", code)

}
