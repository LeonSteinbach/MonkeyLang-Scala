package de.hfu.monkey.objects

import de.hfu.monkey
import de.hfu.monkey.evaluator.Evaluator

object Builtins {
	val builtins: List[(String, BuiltinObject)] = List(
		"len" -> BuiltinObject((args: Array[Option[Object]]) => {
			if (args.length != 1) {
				Some(ErrorObject(s"wrong number of arguments. got ${args.length} but expected 1."))
			} else {
				args.head match {
					case Some(arrayObject: ArrayObject) =>
						Some(IntegerObject(arrayObject.elements.size))
					case Some(stringObject: StringObject) =>
						Some(IntegerObject(stringObject.value.length))
					case obj =>
						Some(ErrorObject(s"argument to `len` not supported, got ${obj.get.`type`()}"))
				}
			}
		}),
		"puts" -> BuiltinObject((args: Array[Option[Object]]) => {
			args.foreach { arg => println(arg.get) }
			None
		}),
		"first" -> BuiltinObject((args: Array[Option[Object]]) => {
			if (args.length != 1) {
				Some(ErrorObject(s"wrong number of arguments. got ${args.length} but expected 1."))
			} else {
				args.head match {
					case Some(arrayObject: ArrayObject) =>
						arrayObject.elements.headOption
					case obj =>
						Some(ErrorObject(s"argument to `first` not supported, got ${obj.get.`type`()}"))
				}
			}
		}),
		"last" -> BuiltinObject((args: Array[Option[Object]]) => {
			if (args.length != 1) {
				Some(ErrorObject(s"wrong number of arguments. got ${args.length} but expected 1."))
			} else {
				args.head match {
					case Some(arrayObject: ArrayObject) =>
						arrayObject.elements.lastOption
					case obj =>
						Some(ErrorObject(s"argument to `last` not supported, got $obj.get.`type`()}"))
				}
			}
		}),
		"rest" -> BuiltinObject((args: Array[Option[Object]]) => {
			if (args.length != 1) {
				Some(ErrorObject(s"wrong number of arguments. got ${args.length} but expected 1."))
			} else {
				args.head match {
					case Some(arrayObject: ArrayObject) =>
						if (arrayObject.elements.nonEmpty) Some(ArrayObject(arrayObject.elements.tail))
						else None
					case obj =>
						Some(ErrorObject(s"argument to `rest` not supported, got ${obj.get.`type`()}"))
				}
			}
		}),
		"push" -> BuiltinObject((args: Array[Option[Object]]) => {
			if (args.length != 2) {
				Some(ErrorObject(s"wrong number of arguments. got ${args.length} but expected 2."))
			} else {
				args.head match {
					case Some(arrayObject: ArrayObject) =>
						val newElements = arrayObject.elements :+ args(1).get
						Some(ArrayObject(newElements))
					case obj =>
						Some(ErrorObject(s"argument to `push` not supported, got ${obj.get.`type`()}"))
				}
			}
		}),
	)

	def getBuiltinByName(name: String): Option[BuiltinObject] = {
		builtins.find {
			case (function, _) =>
				name == function
		}.map(_._2)
	}
}
