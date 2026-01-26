package object mustachio {

  extension (json: ujson.Value) {

    /** Get a field from a JSON object if present
      *
      * @param field
      *   the field name to retrieve
      * @return
      *   an option containing the JSON value if the field is present, otherwise
      *   None
      */
    def ?(field: String): Option[ujson.Value] =
      json match {
        case ujson.Obj(inner) => inner.get(field)
        case _                => Option.empty
      }
  }

  extension (jsonOpt: Option[ujson.Value]) {

    /** Get a field from a JSON object if present
      *
      * @param field
      *   the field name to retrieve
      * @return
      *   an option containing the JSON value if the field is present, otherwise
      *   None
      */
    def ?(field: String): Option[ujson.Value] =
      jsonOpt.flatMap(_ ? field)
  }

}
