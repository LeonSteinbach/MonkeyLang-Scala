package de.hfu.monkey.vm

import de.hfu.monkey.code.*
import de.hfu.monkey.objects.*

case class Frame(function: CompiledFunctionObject, var ip: Int = -1) {
	def instructions: Instructions = function.instructions
}
