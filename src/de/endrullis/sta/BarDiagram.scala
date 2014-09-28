package de.endrullis.sta

import generators.{RiseUp, FadeIn}
import java.util.Locale

/**
 * An (animated) column/bar diagram based on the one described on http://www.statistiker-wg.de/pgf/tutorials/barplot.htm.
 *
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
class BarDiagram(finalValues: List[(String, Double)], xAxisLabel: String = null, yAxisLabel: String = null, printValues: Boolean = true, barWidth: Double = 1, barSpace: Double = 0.5) extends BaseVarIC {

	Locale.setDefault(Locale.ENGLISH)

	val count = finalValues.size
	val names = finalValues.map(_._1).toArray
	val values = finalValues.map(_._2).toArray
	val xIndices = names.indices
	val xWidth = count*barWidth + (count+1)*barSpace
	val yHeight = 4.5

	def barXStart(index: Int) = barSpace + (barWidth+barSpace)*index

	def valuesString(values: Array[Double]) = names.zipWithIndex.map{ case (name, index) => "%f/%f/%.1f/%s".format(barXStart(index), values(index), values(index), name) }.mkString(",\n")

	def xAxis = new RiseUp[AxisRising]({(startTime, duration, timeMap) =>
		implicit val tm = timeMap.as[Pos]
		val xPos = Pos(0,0) start startTime changeTo Pos(xWidth,0) in duration*0.9
		val laPos = Pos(0,0) start startTime stay duration*0.9 changeTo Pos(0,-0.1) in duration*0.1
		val raPos = xPos + laPos

		vi"""\draw (0,0) -- $xPos;
		    |\draw (0,0) -- $laPos;
		    |\draw $xPos -- $raPos;
		    |"""
	})


	def yAxis = new RiseUp[AxisRising]({(startTime, duration, timeMap) =>
		implicit val tm = timeMap.as[Pos]
		val yPos = Pos(-0.1,0) start startTime changeTo Pos(-0.1,yHeight) in duration*0.9
		val aPos = Pos(0,0) start startTime stay duration*0.9 changeTo Pos(-0.1,0) in duration*0.1
		val baPos = Pos(-0.1,0) + aPos
		val taPos = yPos + aPos

		vi"""\draw (-0.1,0) -- $yPos;  %Ordinate
		    |\draw (-0.1,0) -- $baPos;  %unteres Ende der Ordinate
		    |\draw $yPos -- $taPos;  %oberes Ende der Ordinate
		    |"""
	})

	def yHelperLine(yValue: Double) = new RiseUp[AxisRising]({(startTime, duration, timeMap) =>
		implicit val tm = timeMap.as[Double]
		val xScale = 0.0 start startTime changeTo 1.0 in duration
		val xWidth = barSpace + names.size*(barWidth+barSpace)

		vi"""\draw[gray!50, text=black] (-0.2*$xScale, $yValue) -- ($xScale*$xWidth, $yValue);"""
	})


	def xLabel(index: Int) = new FadeIn[LabelFadeIn]({(startTime, duration, timeMap) =>
		implicit val tm = timeMap.as[Double]
		val opacity = 0.0 start startTime changeTo 1.0 in duration
		val xValue = barSpace + index*(barWidth+barSpace)
		val xLabel = names(index)

		vi"""\node[rotate=45, left, opacity=$opacity] at (0.6 + $xValue, -0.1) {$xLabel};"""
	})

	def xLabels = names.indices.map(xLabel)

	def yLabel(yValue: Double) = new FadeIn[LabelFadeIn]({(startTime, duration, timeMap) =>
		implicit val tm = timeMap.as[Double]
		val opacity = 0.0 start startTime changeTo 1.0 in duration

		vi"""\node[opacity=$opacity] at (-0.5, $yValue) {$yValue};"""
	})

	def bar(index: Int) = new RiseUp[BarRising]({(startTime, duration, timeMap) =>
		implicit val tm = timeMap.as[Double]
		val yValue = 0.0 start startTime changeTo values(index) in duration
		val xValue = barSpace + index*(barWidth+barSpace)
		val yText = yValue.map("%.1f".format(_))

		vi"""\draw[fill=barColor] ($xValue, 0) rectangle ($barWidth+$xValue, $yValue) %die Säulen
		    |  node at ($barWidth/2 + $xValue, $yValue + 0.3) {$yText}; %die Prozente über den Säulen
		    |"""
	})


	// TimeMap definitions
	trait LabelFadeIn
	trait AxisRising
	trait BarRising

}
