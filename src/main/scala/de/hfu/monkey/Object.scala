package de.hfu.monkey

object ObjectType extends Enumeration {
	type ObjectType = Value
	val INTEGER, BOOLEAN, STRING, ARRAY, NULL, RETURN, ERROR, FUNCTION, BUILTIN = Value
}

type BuiltinFunction = Array[Object] => Object

trait Object {
	def `type`(): ObjectType.Value
	override def toString: String
}

case class IntegerObject(value: Int) extends Object {
	override def toString: String = value.toString
	def `type`(): ObjectType.Value = ObjectType.INTEGER
}

case class BooleanObject(value: Boolean) extends Object {
	override def toString: String = value.toString
	def `type`(): ObjectType.Value = ObjectType.BOOLEAN
}

case class StringObject(value: String) extends Object {
	override def toString: String = value
	def `type`(): ObjectType.Value = ObjectType.STRING
}

case class ArrayObject(elements: List[Object]) extends Object {
	override def toString: String = s"[${elements.mkString(", ")}]"
	def `type`(): ObjectType.Value = ObjectType.ARRAY
}

case object NullObject extends Object {
	override def toString: String = "null"
	def `type`(): ObjectType.Value = ObjectType.NULL
}

case class ReturnObject(value: Option[Object] = None) extends Object {
	override def toString: String = value.toString
	def `type`(): ObjectType.Value = ObjectType.RETURN
}

case class ErrorObject(message: String) extends Object {
	override def toString: String = s"ERROR: $message"
	def `type`(): ObjectType.Value = ObjectType.ERROR
}

case class FunctionObject(parameters: Option[List[Identifier]] = None, body: Option[BlockStatement] = None, environment: Environment) extends Object {
	override def toString: String = s"fn(${parameters.map(_.mkString(", ")).getOrElse("")}) ${body.getOrElse("")}"
	def `type`(): ObjectType.Value = ObjectType.FUNCTION
}

case class BuiltinObject(builtinFunction: BuiltinFunction) extends Object {
	override def toString: String = "builtin function"
	def `type`(): ObjectType.Value = ObjectType.BUILTIN
}