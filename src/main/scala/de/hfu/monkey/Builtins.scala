package de.hfu.monkey

object Builtins {
	val builtins: Map[String, BuiltinObject] = Map(
		"len" -> BuiltinObject((args: Array[Object]) => {
			if (args.length != 1) {
				ErrorObject(s"wrong number of arguments. got ${args.length} but expected 1.")
			} else {
				args(0).`type`() match {
					//case ObjectType.ARRAY =>
					//	IntegerObject(args(0).asInstanceOf[Array].Elements.size)
					case ObjectType.STRING =>
						IntegerObject(args(0).asInstanceOf[StringObject].value.length)
					case _ =>
						ErrorObject(s"argument to `len` not supported, got ${args(0).`type`()}")
				}
			}
		})
	)
}
