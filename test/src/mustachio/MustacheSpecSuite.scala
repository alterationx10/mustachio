package mustachio

import scala.io.Source
import scala.util.{Try, Using}
import ujson.*
import mustachio.*

case class Spec(
    name: String,
    desc: String,
    data: ujson.Value,
    template: String,
    expected: String,
    partials: ujson.Value = ujson.Null
) derives upickle.Reader

case class SpecSuite(tests: Seq[Spec]) derives upickle.Reader

trait MustacheSpecSuite extends munit.FunSuite {

  def runSpec(
      spec: Spec
  )(implicit loc: munit.Location): Unit = {
    test(spec.name) {
      val context  = Stache.fromJson(spec.data)
      val partials = Option(Stache.fromJson(spec.partials))
      assertEquals(
        Mustachio.render(
          spec.template,
          context,
          partials
        ),
        spec.expected
      )
    }
  }

  /** Attempts to load and parse a Moustache spec suite from a resource file.
    * @return
    */
  def specSuite(resource: String): SpecSuite =
    Using(Source.fromResource(resource)) { source =>
      val stuff = Try(upickle.read[SpecSuite](source.mkString))
      stuff match {
        case scala.util.Failure(exception) => exception.printStackTrace()
        case _                             => ()
      }
      stuff.get
    }
      .getOrElse(throw new Exception("Failed to parse json for specSuite"))
}
