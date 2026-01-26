package mustachio

class MustachioDelimitersSpec extends MustacheSpecSuite {

  specSuite("mustache/delimiters.json").tests
    .foreach(runSpec)

}
