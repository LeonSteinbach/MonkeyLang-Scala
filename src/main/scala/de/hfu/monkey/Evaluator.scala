package de.hfu.monkey

import scala.collection.mutable.ListBuffer
import scala.util.control.NonLocalReturns.*

object Evaluator {
	private val TRUE: BooleanObject = BooleanObject(true)
	private val FALSE: BooleanObject = BooleanObject(false)
	private val NULL: NullObject.type = NullObject

	def evaluateProgram(program: Program, environment: Environment): Object = {
		program.statements.foldLeft(NULL: Object) { (_, statement) =>
			val evaluatedStatement = evaluate(Some(statement), environment)
			evaluatedStatement.`type`() match {
				case ObjectType.RETURN =>
					evaluatedStatement.asInstanceOf[ReturnObject].value.getOrElse(NULL)
				case ObjectType.ERROR => evaluatedStatement
				case _ => evaluatedStatement
			}
		}
	}

	private def evaluate(node: Option[Node], environment: Environment): Object = node match {
		case Some(program: Program) => evaluateProgram(program, environment)
		case Some(expressionStatement: ExpressionStatement) => evaluate(Some(expressionStatement.expression), environment)
		case Some(integerLiteral: IntegerLiteral) => IntegerObject(integerLiteral.value)
		case Some(booleanLiteral: BooleanLiteral) => BooleanObject(booleanLiteral.value)
		case Some(stringLiteral: StringLiteral) => StringObject(stringLiteral.value)
		case Some(prefixExpression: PrefixExpression) => evaluatePrefixExpression(prefixExpression.operator, evaluate(Some(prefixExpression.value), environment))
		case Some(infixExpression: InfixExpression) =>
			val infixLeftValue: Object = evaluate(Some(infixExpression.left), environment)
			if (isError(infixLeftValue))
				infixLeftValue
			else {
				val infixRightValue: Object = evaluate(Some(infixExpression.right), environment)
				if (isError(infixRightValue))
					infixRightValue
				else
					evaluateInfixExpression(infixExpression.operator, infixLeftValue, infixRightValue)
			}
		case Some(blockStatement: BlockStatement) => evaluateBlockStatement(blockStatement, environment)
		case Some(ifExpression: IfExpression) => evaluateIfExpression(ifExpression, environment)
		case Some(returnStatement: ReturnStatement) => evaluateReturnStatement(returnStatement, environment)
		case Some(letStatement: LetStatement) => evaluateLetStatement(letStatement, environment)
		case Some(identifier: Identifier) => evaluateIdentifier(identifier, environment)
		case Some(functionLiteral: FunctionLiteral) => FunctionObject(Some(functionLiteral.parameters), Some(functionLiteral.body), environment)
		case Some(callExpression: CallExpression) => evaluateCallExpression(callExpression, environment)
		case _ => NULL
	}

	private def isError(value: Object): Boolean = value != NULL && value.`type`() == ObjectType.ERROR

	private def evaluateBlockStatement(blockStatement: BlockStatement, environment: Environment): Object = returning {
		var result: Object = NULL
		for (statement <- blockStatement.statements) {
			val evaluatedStatement = evaluate(Some(statement), environment)
			evaluatedStatement.`type`() match {
				case ObjectType.RETURN | ObjectType.ERROR => throwReturn(evaluatedStatement)
				case _ => result = evaluatedStatement
			}
		}
		result
	}

	private def evaluatePrefixExpression(operator: String, right: Object): Object = operator match {
		case "!" if isTruthy(right) => FALSE
		case "!" => TRUE
		case "-" => right.`type`() match {
			case ObjectType.INTEGER => IntegerObject(-right.asInstanceOf[IntegerObject].value)
			case _ => NULL
		}
	}

	private def evaluateInfixExpression(operator: String, left: Object, right: Object): Object = (left, operator, right) match {
		case (left: Object, opr: String, right: Object) if left.`type`() != right.`type`() => ErrorObject(s"type mismatch: ${left.`type`()} $opr ${right.`type`()}")
		case (left: IntegerObject, operator: String, right: IntegerObject) => evaluateIntegerInfixExpression(operator, left, right)
		case (left: StringObject, operator: String, right: StringObject) => evaluateStringInfixExpression(operator, left, right)
		case (left: BooleanObject, "==", right: BooleanObject) => if (left.value == right.value) TRUE else FALSE
		case (left: BooleanObject, "!=", right: BooleanObject) => if (left.value != right.value) TRUE else FALSE
		case _ => ErrorObject(s"unknown operator: ${left.`type`()} $operator ${right.`type`()}")
	}

	private def evaluateIntegerInfixExpression(operator: String, left: IntegerObject, right: IntegerObject): Object = operator match {
		case "+" => IntegerObject(left.value + right.value)
		case "-" => IntegerObject(left.value - right.value)
		case "*" => IntegerObject(left.value * right.value)
		case "/" => IntegerObject(left.value / right.value)
		case "<" => if (left.value < right.value) TRUE else FALSE
		case ">" => if (left.value > right.value) TRUE else FALSE
		case "==" => if (left.value == right.value) TRUE else FALSE
		case "!=" => if (left.value != right.value) TRUE else FALSE
		case _ => ErrorObject(s"unknown operator: ${left.`type`()} $operator ${right.`type`()}")
	}

	private def evaluateStringInfixExpression(operator: String, left: StringObject, right: StringObject): Object = operator match {
		case "+" => StringObject(left.value + right.value)
		case _ => ErrorObject(s"unknown operator: ${left.`type`()} $operator ${right.`type`()}")
	}

	private def evaluateIfExpression(ifExpression: IfExpression, environment: Environment): Object = {
		val condition: Object = evaluate(Some(ifExpression.condition), environment)
		if (isError(condition))
			condition
		else {
			if (isTruthy(condition))
				evaluate(Some(ifExpression.consequence), environment)
			else
				evaluate(Some(ifExpression.alternative), environment)
		}
	}

	private def evaluateReturnStatement(returnStatement: ReturnStatement, environment: Environment): Object = {
		val returnValue = evaluate(Some(returnStatement.value), environment)
		if (isError(returnValue)) returnValue else ReturnObject(Option(returnValue))
	}

	private def evaluateLetStatement(letStatement: LetStatement, environment: Environment): Object = {
		val letValue = evaluate(Some(letStatement.value), environment)
		if (isError(letValue))
			letValue
		else {
			environment.set(letStatement.name.name, letValue)
			NULL
		}
	}

	private def evaluateIdentifier(identifier: Identifier, environment: Environment): Object = {
		val (result, ok) = environment.get(identifier.name)
		if (ok) {
			result.getOrElse(NULL)
		} else {
			val maybeBuiltin = Builtins.builtins.get(identifier.name)
			maybeBuiltin match {
				case Some(builtinObject: BuiltinObject) => Some(builtinObject).get
				case None => ErrorObject(s"identifier not found: ${identifier.name}")
			}
		}
	}

	private def evaluateCallExpression(callExpression: CallExpression, environment: Environment): Object = {
		val callFunction = evaluate(Some(callExpression.function), environment)
		if (isError(callFunction))
			callFunction
		else
			val callArguments: List[Object] = evaluateExpressions(Some(callExpression.arguments), environment)
			if (callArguments.length == 1 && isError(callArguments.head)) callArguments.head else applyFunction(callFunction, callArguments)
	}

	private def evaluateExpressions(expressions: Option[List[Expression]], environment: Environment): List[Object] = returning {
		val result = ListBuffer.empty[Object]

		expressions match {
			case Some(exps) =>
				exps.foreach { expression =>
					val evaluated = evaluate(Some(expression), environment)
					if (isError(evaluated)) {
						throwReturn(List(evaluated))
					} else {
						result += evaluated
					}
				}
			case None =>
		}

		result.toList
	}

	private def applyFunction(function: Object, arguments: List[Object]): Object = {
		function match {
			case functionObject: FunctionObject =>
				val extendedEnvironment = extendFunctionEnvironment(functionObject, arguments)
				val evaluated = functionObject.body match {
					case Some(body) => evaluate(Some(body), extendedEnvironment)
					case _ => return NULL
				}
				unwrapReturnValue(evaluated)
			case builtinObject: BuiltinObject => builtinObject.builtinFunction(arguments.toArray)
			case _ => ErrorObject(s"not a function: ${function.`type`()}")
		}
	}

	private def extendFunctionEnvironment(function: FunctionObject, arguments: List[Object]): Environment = {
		val environment = new Environment(Some(function.environment))
		function.parameters.foreach { parameters =>
			for ((parameter, argument) <- parameters.zip(arguments)) {
				environment.set(parameter.name, argument)
			}
		}
		environment
	}

	private def unwrapReturnValue(obj: Object): Object = obj match {
		case ReturnObject(value) => value.getOrElse(NULL)
		case _ => obj
	}

	private def isTruthy(obj: Object): Boolean = obj match {
		case FALSE | NULL => false
		case _ => true
	}

}
