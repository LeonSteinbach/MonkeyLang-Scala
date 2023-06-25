package de.hfu.monkey.evaluator

import de.hfu.monkey
import de.hfu.monkey.ast.*
import de.hfu.monkey.objects.*
import de.hfu.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.NonLocalReturns.*

object Evaluator {
	private val TRUE: BooleanObject = BooleanObject(true)
	private val FALSE: BooleanObject = BooleanObject(false)
	val NULL: NullObject.type = NullObject

	private def isError(value: Object): Boolean = value != NULL && value.`type`() == ObjectType.ERROR

	def evaluate(node: Option[Node], env: Environment): Object = node match {
		case Some(node: Program) => evaluateProgram(node, env)
		case Some(node: ExpressionStatement) => evaluateExpressionStatement(node, env)
		case Some(node: IntegerLiteral) => evaluateInteger(node)
		case Some(node: BooleanLiteral) => evaluateBoolean(node)
		case Some(node: StringLiteral) => evaluateString(node)
		case Some(node: ArrayLiteral) => evaluateArray(node, env)
		case Some(node: HashLiteral) => evaluateHash(node, env)
		case Some(node: PrefixExpression) => evaluatePrefixExpression(node, env)
		case Some(node: InfixExpression) => evaluateInfixExpression(node, env)
		case Some(node: BlockStatement) => evaluateBlockStatement(node, env)
		case Some(node: IfExpression) => evaluateIfExpression(node, env)
		case Some(node: ReturnStatement) => evaluateReturnStatement(node, env)
		case Some(node: LetStatement) => evaluateLetStatement(node, env)
		case Some(node: Identifier) => evaluateIdentifier(node, env)
		case Some(node: FunctionLiteral) => evaluateFunctionLiteral(node, env)
		case Some(node: CallExpression) => evaluateCallExpression(node, env)
		case Some(node: IndexExpression) => evaluateIndexExpression(node, env)
		case _ => NULL
	}

	private def evaluateProgram(program: Program, environment: Environment): Object = {
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

	private def evaluateExpressionStatement(statement: ExpressionStatement, environment: Environment): Object = {
		evaluate(Some(statement.expression), environment)
	}

	private def evaluateInteger(integerLiteral: IntegerLiteral): Object = {
		IntegerObject(integerLiteral.value)
	}

	private def evaluateBoolean(booleanLiteral: BooleanLiteral): Object = {
		BooleanObject(booleanLiteral.value)
	}

	private def evaluateString(stringLiteral: StringLiteral): Object = {
		StringObject(stringLiteral.value)
	}

	private def evaluateArray(arrayLiteral: ArrayLiteral, environment: Environment): Object = {
		val elements: List[Option[Object]] = evaluateExpressions(Some(arrayLiteral.elements), environment)
		if (elements.length == 1 && isError(elements.head.get)) elements.head.get else ArrayObject(elements.map(el => el.get))
	}

	private def evaluateHash(hashLiteral: HashLiteral, environment: Environment): Object = returning {
		val pairs = mutable.HashMap.empty[HashKey, HashPair]
		hashLiteral.pairs.foreach { (keyNode, valueNode) =>
			val key = evaluate(Some(keyNode), environment)
			if (isError(key)) {
				throwReturn(key)
			}
			key match {
				case hashable: Hashable =>
					val value = evaluate(Some(valueNode), environment)
					if (isError(value)) {
						throwReturn(value)
					}
					pairs += (hashable.hashKey -> HashPair(hashable, value))
				case _ => throwReturn(ErrorObject(s"unusable as a hash key: ${key.`type`()}"))
			}
		}
		HashObject(pairs.toMap)
	}

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

	private def evaluatePrefixExpression(prefixExpression: PrefixExpression, environment: Environment): Object = {
		val right: Object = evaluate(Some(prefixExpression.value), environment)

		prefixExpression.operator match {
			case "!" if isTruthy(right) => FALSE
			case "!" => TRUE
			case "-" => right match {
				case right: IntegerObject => IntegerObject(-right.value)
				case _ => NULL
			}
		}
	}

	private def evaluateInfixExpression(infixExpression: InfixExpression, environment: Environment): Object = {
		val infixLeftValue: Object = evaluate(Some(infixExpression.left), environment)
		if (isError(infixLeftValue))
			return infixLeftValue
		
		val infixRightValue: Object = evaluate(Some(infixExpression.right), environment)
		if (isError(infixRightValue))
			return infixRightValue

		(infixLeftValue, infixExpression.operator, infixRightValue) match {
			case (left: Object, opr: String, right: Object)
				if left.`type`() != right.`type`() =>
					ErrorObject(s"type mismatch: ${left.`type`()} $opr ${right.`type`()}")
			case (left: IntegerObject, operator: String, right: IntegerObject) =>
				evaluateIntegerInfixExpression(operator, left, right)
			case (left: StringObject, operator: String, right: StringObject) =>
				evaluateStringInfixExpression(operator, left, right)
			case (left: BooleanObject, "==", right: BooleanObject) =>
				if (left.value == right.value) TRUE else FALSE
			case (left: BooleanObject, "!=", right: BooleanObject) =>
				if (left.value != right.value) TRUE else FALSE
			case (left, operator, right) =>
				ErrorObject(s"unknown operator: ${left.`type`()} $operator ${right.`type`()}")
		}
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
		val returnValue: Object = evaluate(Some(returnStatement.value), environment)
		if (isError(returnValue)) returnValue else ReturnObject(Option(returnValue))
	}

	private def evaluateLetStatement(letStatement: LetStatement, environment: Environment): Object = {
		val letValue: Object = evaluate(Some(letStatement.value), environment)
		if (isError(letValue))
			letValue
		else {
			environment.set(letStatement.identifier.name, letValue)
			NULL
		}
	}

	private def evaluateIdentifier(identifier: Identifier, environment: Environment): Object = {
		val (result: Option[Object], ok: Boolean) = environment.get(identifier.name)
		if (ok) {
			result.getOrElse(NULL)
		} else {
			val maybeBuiltin = Builtins.getBuiltinByName(identifier.name)
			maybeBuiltin match {
				case Some(builtinObject: BuiltinObject) => Some(builtinObject).get
				case None => ErrorObject(s"identifier not found: ${identifier.name}")
			}
		}
	}

	private def evaluateFunctionLiteral(functionLiteral: FunctionLiteral, environment: Environment): Object = {
		FunctionObject(Some(functionLiteral.parameters), Some(functionLiteral.body), environment)
	}

	private def evaluateCallExpression(callExpression: CallExpression, environment: Environment): Object = {
		val callFunction: Object = evaluate(Some(callExpression.function), environment)
		if (isError(callFunction))
			return callFunction

		val callArguments: List[Option[Object]] = evaluateExpressions(Some(callExpression.arguments), environment)

		if (callArguments.length == 1 && isError(callArguments.head.get))
			callArguments.head.get
		else
			applyFunction(callFunction, callArguments).getOrElse(NULL)
	}

	private def evaluateIndexExpression(indexExpression: IndexExpression, environment: Environment): Object = {
		val left = evaluate(Some(indexExpression.left), environment)
		if (isError(left)) return left

		val index = evaluate(Some(indexExpression.index), environment)
		if (isError(index)) return index

		(left, index) match {
			case (arrayObject: ArrayObject, integerValue: IntegerObject) =>
				val idx = integerValue.value
				val elements = arrayObject.elements
				if (idx < 0 || idx > elements.length - 1) NULL else elements(idx)

			case (hashObject: HashObject, hashable: Hashable) =>
				val hashKey = HashKey(hashable.hashKey.value)
				hashObject.pairs.get(hashKey).map(_.value).getOrElse(NULL)

			case _ => ErrorObject(s"index operator not supported: ${left.`type`()}")
		}
	}

	private def evaluateExpressions(expressions: Option[List[Expression]], environment: Environment): List[Option[Object]] = returning {
		val result: ListBuffer[Option[Object]] = ListBuffer.empty[Option[Object]]

		expressions match {
			case Some(exps) =>
				exps.foreach { expression =>
					val evaluated = evaluate(Some(expression), environment)
					if (isError(evaluated)) {
						throwReturn(List(Some(evaluated)))
					} else {
						result += Some(evaluated)
					}
				}
			case None =>
		}

		result.toList
	}

	private def applyFunction(function: Object, arguments: List[Option[Object]]): Option[Object] = {
		function match {
			case functionObject: FunctionObject =>
				val extendedEnvironment = extendFunctionEnvironment(functionObject, arguments)
				val evaluated = functionObject.body match {
					case Some(body) => evaluate(Some(body), extendedEnvironment)
					case _ => return Some(NULL)
				}
				Some(unwrapReturnValue(Some(evaluated)))
			case builtinObject: BuiltinObject => builtinObject.builtinFunction(arguments.toArray)
			case _ => Some(ErrorObject(s"not a function: ${function.`type`()}"))
		}
	}

	private def extendFunctionEnvironment(function: FunctionObject, arguments: List[Option[Object]]): Environment = {
		val environment = new Environment(Some(function.environment))
		function.parameters.foreach { parameters =>
			for ((parameter, argument) <- parameters.zip(arguments)) {
				environment.set(parameter.name, argument.get)
			}
		}
		environment
	}

	private def unwrapReturnValue(obj: Option[Object]): Object = obj match {
		case Some(ReturnObject(value)) => value.getOrElse(NULL)
		case Some(obj) => obj
		case None => NULL
	}

	private def isTruthy(obj: Object): Boolean = obj match {
		case FALSE | NULL => false
		case _ => true
	}

}
