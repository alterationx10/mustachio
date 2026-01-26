package mustachio

class MustachioInvertedSpec extends MustacheSpecSuite {

  specSuite("mustache/inverted.json").tests
    .foreach(runSpec)

}
