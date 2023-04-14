package de.hfu.monkey.vm

import de.hfu.monkey.code.*
import de.hfu.monkey.objects.*

case class Frame(closure: ClosureObject, var basePointer: Int, var ip: Int = -1) {
	def instructions: Instructions = closure.function.instructions
}
