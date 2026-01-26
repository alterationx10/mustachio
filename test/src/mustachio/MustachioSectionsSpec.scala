package mustachio

class MustachioSectionsSpec extends MustacheSpecSuite {

  specSuite("mustache/sections.json").tests
    .foreach(runSpec)

}
