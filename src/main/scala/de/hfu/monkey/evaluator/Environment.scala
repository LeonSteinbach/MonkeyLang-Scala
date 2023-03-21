package de.hfu.monkey.evaluator

import scala.collection.mutable

class Environment(var outer: Option[Environment] = None) {
	private val store: mutable.Map[String, Object] = mutable.Map()

	def get(name: String): (Option[Object], Boolean) = {
		store.get(name) match {
			case Some(value) => (Some(value), true)
			case None => outer match {
				case Some(env) => env.get(name)
				case None => (None, false)
			}
		}
	}

	def set(name: String, value: Object): Any = {
		store(name) = value
		value
	}
}
