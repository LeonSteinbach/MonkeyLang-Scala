package de.hfu.monkey.objects

import de.hfu.monkey.*
import de.hfu.monkey.ast.*
import de.hfu.monkey.evaluator.*
import de.hfu.monkey.code.*

enum ObjectType {
	case INTEGER, BOOLEAN, STRING, ARRAY, HASH, NULL, RETURN, ERROR, FUNCTION, COMPILED_FUNCTION, BUILTIN, CLOSURE
}

type BuiltinFunction = Array[Option[Object]] => Option[Object]

trait Object {
	def `type`(): ObjectType
	override def toString: String
}

case class HashKey(value: Int)

case class HashPair(key: Object, value: Object)

trait Hashable extends Object {
	def hashKey: HashKey
}

case class IntegerObject(value: Int) extends Object with Hashable {
	override def toString: String = value.toString
	def `type`(): ObjectType = ObjectType.INTEGER

	override def hashKey: HashKey = HashKey(value.hashCode())
}

case class BooleanObject(value: Boolean) extends Object with Hashable {
	override def toString: String = value.toString
	def `type`(): ObjectType = ObjectType.BOOLEAN

	override def hashKey: HashKey = HashKey(value.hashCode())
}

case class StringObject(value: String) extends Object with Hashable {
	override def toString: String = value
	def `type`(): ObjectType = ObjectType.STRING

	override def hashKey: HashKey = HashKey(value.hashCode)
}

case class ArrayObject(elements: List[Object]) extends Object {
	override def toString: String = s"[${elements.mkString(", ")}]"
	def `type`(): ObjectType = ObjectType.ARRAY
}

case class HashObject(pairs: Map[HashKey, HashPair]) extends Object {
	override def toString: String = s"{${pairs.values.map(pair => s"${pair.key.toString}: ${pair.value.toString}").mkString(", ")}}"
	def `type`(): ObjectType = ObjectType.HASH
}

case object NullObject extends Object {
	override def toString: String = "null"
	def `type`(): ObjectType = ObjectType.NULL
}

case class ReturnObject(value: Option[Object] = None) extends Object {
	override def toString: String = value.toString
	def `type`(): ObjectType = ObjectType.RETURN
}

case class ErrorObject(message: String) extends Object {
	override def toString: String = s"ERROR: $message"
	def `type`(): ObjectType = ObjectType.ERROR
}

case class FunctionObject(parameters: Option[List[Identifier]] = None, body: Option[BlockStatement] = None, environment: Environment) extends Object {
	override def toString: String = s"fn(${parameters.map(_.mkString(", ")).getOrElse("")}) ${body.getOrElse("")}"
	def `type`(): ObjectType = ObjectType.FUNCTION
}

case class CompiledFunctionObject(instructions: Instructions, numLocals: Int = 0, numParameters: Int = 0) extends Object {
	override def toString: String = s"compiled_function[${instructions.toList}]"
	def `type`(): ObjectType = ObjectType.COMPILED_FUNCTION
}

case class BuiltinObject(builtinFunction: BuiltinFunction) extends Object {
	override def toString: String = "builtin function"
	def `type`(): ObjectType = ObjectType.BUILTIN
}

case class ClosureObject(function: CompiledFunctionObject, free: List[Object] = List.empty) extends Object {
	override def toString: String = "closure"
	def `type`(): ObjectType = ObjectType.CLOSURE
}
