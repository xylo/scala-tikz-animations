package de.endrullis.sta

import collection.mutable
import java.io.{PrintStream, File}
import java.util.regex.Matcher
import io.Source
import sys.process.Process

/**
 * Base class for creating Tikz animations.
 * Just inherit your animation description from this class.
 *
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
class BaseTikzAni extends App with MutableCodeContainer {

	/** Animation settings. */
	object conf {
		/** TeX header to be used. */
		var header = ""
		/** Bounding box of the tikz figure. */
		var boundingBox = (Pos(-1,-1), Pos(1,1))
		/** Frame rate of the animation. */
		var frameRate = 1d
		/** TeX template used to compile the multi-frame animation. */
		var template = "templates/default.template"
		/** TeX template used to generate one animated slide based on the previously generated multi-frame animation. */
		var slideTemplate = "templates/default-slide.template"
	}

	/** Prints the drawing commands on console. */
	def print() {
		print(System.out)
	}

	/** Prints the drawing commands to the given PrintStream. */
	def print(out: PrintStream) {
		val interval = timeInterval

		out.println("% frame rate: " + conf.frameRate)

		var time = interval.get.start
		while (time < interval.get.end) {
			out.println("% time: " + time)
			out.println(generate(time).get)
			out.println("""\newframe""")
			time += 1/conf.frameRate
		}
	}

	/** Returns the frames of the animation. */
	def frames(withLastFrame: Boolean, renderer: (Double, String) => String = (time, content) => content) = {
		val list = new mutable.MutableList[String]

		val interval = timeInterval

		def frameCode(time: Double) = renderer(time, generate(time).get)

		interval match {
			case Some(inter) =>
				var time = interval.get.start
				while (time < interval.get.end) {
					list += frameCode(time)
					time += 1/conf.frameRate
				}
				if (withLastFrame) {
					list += frameCode(interval.get.end)
				}
				list.toList

			case None =>
				List(frameCode(0))
		}
	}

	/** Compiles the animation.
	  * @param createSlide creates also a slide with the animation if not null
	  */
	def animate(createSlide: SlideOptions = null) {
		animateNotInLine("tex/" + getClass.getName.replaceAll("\\$$", ""), createSlide)
	}

	/** Compiles the animation.
	  * @param filename filename (without extension) of the target tex file
	  * @param createSlide creates also a slide with the animation if not null
	  */
	def animateNotInLine(filename: String, createSlide: SlideOptions = null) {
		val file = new File(filename + ".tex")
		file.getAbsoluteFile.getParentFile.mkdirs()

		val withLastFrame = Option(createSlide).map(!_.options.contains("loop")).getOrElse(true)

		def renderer (time: Double, content: String) = "% time: " + time + "\n" + generate(time).get

		val framesCode = frames(withLastFrame, renderer).map(Matcher.quoteReplacement).mkString("$1", "$2\n$1", "$2")

		val templateText = Source.fromFile(conf.template).getLines().mkString("\n")
		val content = templateText.
			replaceAll("""\$\{BB_POS1\}""", conf.boundingBox._1.toString).
			replaceAll("""\$\{BB_POS2\}""", conf.boundingBox._2.toString).
			replaceAll("(?s)%BEGIN_FRAME(.*)%CODE(.*)%END_FRAME", Matcher.quoteReplacement(conf.header) + "\n" + framesCode)

		printToFile(file){ _.println(content) }

		Process(Seq("pdflatex", file.getName), file.getParentFile).!

		if (createSlide != null) {
			createSlideFromAnimation(filename, createSlide)
		}
	}

	/** Creates a slide from the animation.
		* @param filename filename of the original multi-frame animation
	  * @param slide slide options
	  */
	def createSlideFromAnimation(filename: String, slide: SlideOptions) {
		val file = new File(filename + "-slide.tex")
		val templateText = Source.fromFile(conf.slideTemplate).getLines().mkString("\n")

		val frameRate: Int = if (slide.frameRate == 0) conf.frameRate.toInt else slide.frameRate

		val aniFile = new File(filename).getName

		def animate(startFrame: String = "", endFrame: String = "") = "\\animategraphics["+slide.options+"]{"+frameRate+"}{"+aniFile+"}{"+startFrame+"}{"+endFrame+"}"

		val aniContent = if (pauses.isEmpty) {
			animate()
		} else {
			(0.0 :: pauses.toList.sorted.toList ::: List(timeInterval.get.end)).sliding(2).map{ case List(start, end) =>
				animate((start*frameRate).toInt.toString, (end*frameRate).toInt.toString)
			}.zipWithIndex.map{case (code, index) => "\\only<"+(index+1)+">{"+code+"}%"}.mkString("\n")
		}

		val frameContent = {
			Option(slide.title).map("\\frametitle" + _ + "}\n\n").getOrElse("") +
			"\\begin{center}\n" + aniContent + "\n\\end{center}\n"
		}

		val content = templateText.replaceAll("""\$\{FRAME_CONTENT\}""", Matcher.quoteReplacement(aniContent)+"\n")

		printToFile(file)(_.println(content))

		Process(Seq("pdflatex", file.getName), file.getParentFile).!
	}

	def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
	  val p = new java.io.PrintWriter(f)
	  try { op(p) } finally { p.close() }
	}

}

/**
 * Slide options.
 *
 * @param title frame title
 * @param options tex options for the included animation (e.g. with, autoplay, palindrome, ...)
 * @param frameRate frame rate of the animation (if it shall be different from the frame rate of the animation)
 */
case class SlideOptions(title: String = null, options: String = "width=11cm,autoplay,palindrome", frameRate: Int = 0)
