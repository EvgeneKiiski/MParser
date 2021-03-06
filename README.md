# MParser

MParser is simple scala parser combinator.

## Getting MParser

The current stable version is 0.0.6.

If you're using SBT, add the following line to your build file:

```scala
resolvers += "MParser.org" at "http://repository.mparser.org/"
libraryDependencies += "org.mparser" %% "mparser" % "0.0.6"
```

## Quick Start

```scala
import org.mparser.MParser
import org.mparser.MParser._

val text ="""
              You can solve this problem in several different ways.
              First: read same text
    """

val delimiter = space() <|> char('.') <|> char(':') <|> char('\n')

val token = (skipMany1(delimiter) >> many1(letterOrDigit())).map(_.mkString)

val result = many1(token).run(text.toStream)

println(result)
```

## Resources

The [Scaladocs](http://scaladoc.mparser.org/).

The [examples module](https://github.com/EvgeneKiiski/MParser/tree/master/examples/src/main/scala/org/mparser/test).
