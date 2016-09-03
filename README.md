scala-tikz-animations
=====================

This project provides a Scala internal DSL (Scala API) for creating LaTeX (Beamer) animations.

You can either build normal Beamer animations consisting of multiple slides
or convert them into pdf animations that can be imported in LaTeX again.
Note that only a few pdf readers are currently able to show such pdf animations.
Actually, I'm only aware of Adobe Reader having this functionality.


## Usage guide

In order to use the scala-tikz-animations API you have to include the following
maven dependency:

```xml
<dependency>
	<groupId>de.endrullis.sta</groupId>
	<artifactId>scala-tikz-animations</artifactId>
	<version>0.9-SNAPSHOT</version>
</dependency>
```

Moreover, I strongly suggest you to use a Scala IDE instead of a simple text
editor.

## Timed variables

For understanding 

##


## Slide examples

### Hello World

```scala
import de.endrullis.sta._

/**
 * Hello World example.
 *
 * @author Stefan Endrullis &lt;stefan@endrullis.de&gt;
 */
object HelloWorld extends ScalaTikzAni {

	// show "Hello World" only for one moment (0 seconds)
	add("\\node at (0,0) {Hello World};" stay 0)

	// generate the pdf
	animate()

}
```
