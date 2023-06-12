package de.hfu.monkey.evaluator

import de.hfu.monkey
import de.hfu.monkey.objects
import de.hfu.monkey.objects.{ArrayObject, BuiltinObject, ErrorObject, IntegerObject, NullObject, ObjectType, StringObject}

object Builtins {
	val builtins: Map[String, BuiltinObject] = Map(
		"len" -> BuiltinObject((args: Array[objects.Object]) => {
			if (args.length != 1) {
				ErrorObject(s"wrong number of arguments. got ${args.length} but expected 1.")
			} else {
				args(0) match {
					case arrayObject: ArrayObject =>
						IntegerObject(arrayObject.elements.size)
					case stringObject: StringObject =>
						IntegerObject(stringObject.value.length)
					case _ =>
						ErrorObject(s"argument to `len` not supported, got ${args(0).`type`()}")
				}
			}
		}),
		"first" -> BuiltinObject((args: Array[monkey.objects.Object]) => {
			if (args.length != 1) {
				ErrorObject(s"wrong number of arguments. got ${args.length} but expected 1.")
			} else {
				args(0) match {
					case arrayObject: ArrayObject =>
						if (arrayObject.elements.nonEmpty) arrayObject.elements.head
						else NullObject
					case _ =>
						ErrorObject(s"argument to `first` not supported, got ${args(0).`type`()}")
				}
			}
		}),
		"last" -> BuiltinObject((args: Array[monkey.objects.Object]) => {
			if (args.length != 1) {
				ErrorObject(s"wrong number of arguments. got ${args.length} but expected 1.")
			} else {
				args(0) match {
					case arrayObject: ArrayObject =>
						if (arrayObject.elements.nonEmpty) arrayObject.elements.last
						else NullObject
					case _ =>
						ErrorObject(s"argument to `last` not supported, got ${args(0).`type`()}")
				}
			}
		}),
		"rest" -> BuiltinObject((args: Array[monkey.objects.Object]) => {
			if (args.length != 1) {
				ErrorObject(s"wrong number of arguments. got ${args.length} but expected 1.")
			} else {
				args(0) match {
					case arrayObject: ArrayObject =>
						if (arrayObject.elements.nonEmpty) ArrayObject(arrayObject.elements.tail)
						else NullObject
					case _ =>
						ErrorObject(s"argument to `rest` not supported, got ${args(0).`type`()}")
				}
			}
		}),
		"push" -> BuiltinObject((args: Array[monkey.objects.Object]) => {
			if (args.length != 2) {
				ErrorObject(s"wrong number of arguments. got ${args.length} but expected 2.")
			} else {
				args(0) match {
					case arrayObject: ArrayObject =>
						val newElements = arrayObject.elements :+ args(1)
						ArrayObject(newElements)
					case _ =>
						ErrorObject(s"argument to `push` not supported, got ${args(0).`type`()}")
				}
			}
		}),
		"puts" -> monkey.objects.BuiltinObject((args: Array[monkey.objects.Object]) => {
			args.foreach { arg => println(arg) }
			Evaluator.NULL
		})
	)
}
