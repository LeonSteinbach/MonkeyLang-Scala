package de.hfu.monkey

object ObjectType extends Enumeration {
	type ObjectType = Value
	val INTEGER, BOOLEAN, STRING, NULL, RETURN, ERROR, FUNCTION = Value
}

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